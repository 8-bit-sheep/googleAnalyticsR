#' ga v4 parse batching
#' @keywords internal
google_analytics_4_parse_batch <- function(response_list){
  
  if(!is.null(response_list$reports)){
    parsed <- lapply(response_list$reports, google_analytics_4_parse)
  } else {
    warning("No $reports found.")
  }

  
  parsed
  
}

#' ga v4 data parsing
#'
#' x is response_list$reports[[1]] from google_analytics_4_parse_batch
#' @importFrom stats setNames
#' @keywords internal
google_analytics_4_parse <- function(x){
  
  myMessage("Parsing GA API v4", level = 1)
 
  #### x <- ga_data2$reports[[1]]
  
  columnHeader <- x$columnHeader
  data <- x$data$rows
  hasDateComparison <- if(length(data[[1]]$metrics) == 2) TRUE else FALSE
  
  if(!is.null(x$data$isDataGolden) && !x$data$isDataGolden){
      warning("Data is not Golden - may change on subsequent API calls.")
  }
  
  if(!is.null(x$data$filteredForPrivacyReasons)){
    warning("Some data has been filtered for privacy reasons.")
  }
  
  dim_names <- unlist(columnHeader$dimensions)
  met_names <- unlist(lapply(columnHeader$metricHeader$metricHeaderEntries, function(x) x$name))
  met_names1 <- gsub("ga:","",met_names)
  # met_types <- unlist(lapply(columnHeader$metricHeader$metricHeaderEntries, function(x) x$type))
  
  if(is.null(data)){
    myMessage("No data found", level = 1)
    return(NULL)
  }
  
  if(!is.null(dim_names)){
    dims <- matrix(unlist(lapply(data, function(x) x$dimensions)),
                   ncol = length(dim_names), byrow = TRUE)
  } else {
    dims <- NULL
  }

  mets <- matrix(unlist(lapply(data, function(x) x$metrics[[1]]$values)),
                 ncol = length(met_names), byrow = TRUE)
  
  ## comparison date metrics
  if(hasDateComparison){
    mets2 <- matrix(unlist(lapply(data, function(x) x$metrics[[2]]$values)),
                    ncol = length(met_names), byrow=TRUE)
    mets <- cbind(mets, mets2)
    met_names <- c(paste0(met_names, ".d1"), paste0(met_names, ".d2"))
  }
  
  ## construct the dataframe
  out <- data.frame(cbind(dims, mets),
                    stringsAsFactors = FALSE, row.names = 1:nrow(mets))
  
  out_names <- c(dim_names, met_names)
  out_names <- gsub("ga:","",out_names)
  names(out) <- out_names
  
  ## type conversion
  met_names <- gsub("ga:","",met_names)
  out[,met_names] <- as.numeric(as.character(unlist(out[,met_names])))
  
  if('date' %in% colnames(out)) {
    out[,'date'] <- as.Date(unlist(out[,'date']), format="%Y%m%d")
  }
  
  ## add support for met_types == TIME
  
  pivot_entries <- pivot_ga4_parse(x, hasDateComparison)
  
  if(!is.null(pivot_entries)) out <- cbind(out, pivot_entries)
  
  totals <- lapply(x$data$totals, function(x) setNames(x$values, met_names1))
  minimums <- lapply(x$data$minimums, function(x) setNames(x$values, met_names1))
  maximums <- lapply(x$data$maximums, function(x) setNames(x$values, met_names1))
  
  attr(out, "totals") <- totals
  attr(out, "minimums") <- minimums
  attr(out, "maximums") <- maximums
  attr(out, "isDataGolden") <- x$data$isDataGolden
  attr(out, "rowCount") <- x$data$rowCount
  attr(out, "samplesReadCounts") <- x$data$samplesReadCounts
  attr(out, "samplingSpaceSizes") <- x$data$samplingSpaceSizes
  attr(out, "nextPageToken") <- x$nextPageToken
  
  assertthat::assert_that(is.data.frame(out))
  
  out
  
}

sampling_message <- function(samplesReadCounts, samplingSpaceSizes, hasDateComparison = FALSE){
  samplePercent <-  100
  if(!is.null(samplesReadCounts)){
    samplePercent <- get_samplePercent(samplesReadCounts[[1]], samplingSpaceSizes[[1]])
    myMessage("Data is sampled, based on ", samplePercent, "% of sessions.", level = 3)
    
    if(hasDateComparison){
      samplePercent <- get_samplePercent(samplesReadCounts[[2]], samplingSpaceSizes[[2]])
      myMessage("Data Comparison is sampled, based on ", samplePercent, "% of sessions.", level = 3 )
    }
  }
}

get_samplePercent <- function(sampleReadCounts, samplingSpaceSizes){
  sampleReadCounts   <- as.numeric(sampleReadCounts)
  samplingSpaceSizes <- as.numeric(samplingSpaceSizes)

  if(sampleReadCounts == 0 || 
     samplingSpaceSizes == 0 || 
     identical(sampleReadCounts, numeric(0)) || 
     identical(samplingSpaceSizes, numeric(0))) return(numeric(0))

  round(100 * (sampleReadCounts / samplingSpaceSizes), 1)
}

#' New parse GA account summary
#' 
#' @param x The account summary items
#' @keywords internal
#' @importFrom dplyr transmute mutate select
#' @importFrom tidyr unnest
#' @importFrom purrr map_if
parse_ga_account_summary <- function(x){

  ## hack to get rid of global variables warning
  id <- name <- webProperties <- kind <- profiles <- NULL
  x$items %>%
    transmute(accountId = id,
              accountName = name,
              ## fix bug if webProperties is NULL
              webProperties = purrr::map_if(webProperties, is.null, ~ data.frame())) %>% 
    unnest() %>% ##unnest webprops
    mutate(webPropertyId = id,
           webPropertyName = name,
           ## fix bug if profiles is NULL
           profiles = purrr::map_if(profiles, is.null, ~ data.frame())) %>% 
    select(-kind, -id, -name) %>% 
    unnest() %>% ## unnest profiles
    mutate(viewId = id,
           viewName = name) %>% 
    select(-kind, -id, -name)
  
}


parse_google_analytics <- function(x){

  myMessage("Request to profileId: ", x$profileInfo$profileId,
          #     " accountId: ", x$profileInfo$accountId,
          #     " webPropertyId: ", x$profileInfo$webPropertyId,
          " (", x$profileInfo$profileName, ")", level = 3)
  
  if(!is.null(x$error)){
    stop(x$error$message)
  }

  samplePercent <-  100
  if(!is.null(x$containsSampledData)){
    if(x$containsSampledData) {
      samplePercent <- round(100 * (as.numeric(x$sampleSize) / as.numeric(x$sampleSpace)), 2)
      myMessage("Data is sampled, based on ", samplePercent, "% of visits. Use samplingLevel='WALK' to mitigate it.", level = 3 )
    }
  }

  if(x$kind == "analytics#gaData"){
    gadata <- parse_google_analytics_ga(x)
  } else if(x$kind == "analytics#mcfData"){
    gadata <- parse_google_analytics_mcf(x)
  }

  myMessage("Fetched: ",
          paste(colnames(gadata), collapse = " "),
          ". [", NROW(gadata), "] total results out of a possible [", x$totalResults, "], Start-Index: ", x$query$`start-index`, level = 3)

  attr(gadata, "containsSampledData") <- x$containsSampledData
  attr(gadata, "samplePercent") <- samplePercent
  attr(gadata, "samplingLevel") <- x$query$samplingLevel
  attr(gadata, "profileInfo") <- x$profileInfo
  attr(gadata, "dateRange") <- list(startDate = x$query$`start-date`, endDate = x$query$`end-date`)
  attr(gadata, "totalResults") <- x$totalResults

  gadata
}

parse_google_analytics_ga <- function(x){
  gadata <- data.frame(x$rows, stringsAsFactors = F)

  if(nrow(gadata) == 0){
    warning("No data found")
    return(gadata)
  }

  colnames(gadata) <- gsub("ga:", "",x$columnHeaders$name)

  ## changes all metrics into numeric columns
  ## Metrics can be float, integer or currency,
  ## but we just turn them all into numerics.
  mets <- x$columnHeaders[!x$columnHeaders$columnType %in% "DIMENSION",'name']
  mets <- gsub("ga:", "",mets)
  gadata[,mets] <- as.numeric(as.character(unlist(gadata[,mets])))

  ## Date objects.
  if('date' %in% colnames(gadata)) {
    gadata[,'date'] <- as.Date(unlist(gadata[,'date']), format="%Y%m%d")
  }

  gadata

}

## from https://bitbucket.org/unikum/rga/src/b5c0cf89607707a5e3e1ca528e92f1f8fa714e0a/R/convert.R?at=master
parse_google_analytics_mcf <- function(x){

  # Build data.frame for mcf report
  if (is.list(x$rows[[1L]]) && !is.data.frame(x$rows[[1L]]))
    x$rows <- do.call(c, x$rows)

  names <- gsub("^mcf:", "", x$columnHeaders$name)
  types <- x$columnHeaders$dataType

  if ("MCF_SEQUENCE" %in% types && !is.null(x$rows)) {
    ## take out null object lists that the JSON sometimes strangely returns
    type_check <- vapply(x$rows, function(x) inherits(x[1]$conversionPathValue[[1]], "list"), FUN.VALUE=TRUE)
    x$rows <- x$rows[!type_check]
    
    pv_idx <- grep("MCF_SEQUENCE", types, fixed = TRUE, invert = TRUE)
    cv_idx <- grep("MCF_SEQUENCE", types, fixed = TRUE)
    primitive <- lapply(x$rows,
                        function(i) .subset2(i, "primitiveValue")[pv_idx])
    primitive <- do.call(rbind, primitive)
    colnames(primitive) <- names[pv_idx]
    conversion <- lapply(x$rows,
                         function(i) .subset2(i, "conversionPathValue")[cv_idx])
    conversion <- lapply(conversion,
                         function(i) lapply(i, function(j) paste(apply(j, 1, paste, collapse = ":"), collapse = " > ")))
    conversion <- do.call(rbind, lapply(conversion, unlist))
    colnames(conversion) <- names[cv_idx]
    data_df <- data.frame(primitive, conversion, stringsAsFactors = FALSE)[, names]
  } else {
    # empty data.frame if no rows available
    if (is.null(x$rows)) {
      data_df <- data.frame(matrix(ncol = length(names), nrow = 0))
    } else {
      data_df <- as.data.frame(do.call(rbind, lapply(x$rows, unlist)), stringsAsFactors = FALSE)
    }
    
    colnames(data_df) <- names
  }
  return(data_df)

}

parse_google_analytics_meta <- function(x){

  dim_mets <- x$items$id

  dim_mets_attr <- x$items$attributes

  data.frame(name=dim_mets, dim_mets_attr, stringsAsFactors = F)

}