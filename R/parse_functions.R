#' ga v4 parse batching
#'
google_analytics_4_parse_batch <- function(response_list){
  
  parsed <- lapply(response_list$reports, google_analytics_4_parse)
  
  parsed
  
}

#' seeing if this is better
google_analytics_4_parse_tidyjson <- function(x){
  
  json_raw <- jsonlite::toJSON(x, auto_unbox = TRUE)
  reference <- json_raw %>% jsonlite::prettify()
  class(json_raw) <- c(class(json_raw), "character")
  tidy_json <- json_raw %>% as.tbl_json()
  
  headers_json <- tidy_json %>% enter_object("columnHeader")
  
  headers_metrics <- headers_json %>%
    enter_object("metricHeader") %>%
    enter_object("metricHeaderEntries") %>%
    gather_array() %>%
    spread_values(metrics = jstring("name"),
                  type = jstring("type"))
  headers_metrics <- headers_metrics[,setdiff(names(headers_metrics), 
                                              c("document.id","array.index")), drop=FALSE]
  
  headers_dims <- headers_json %>%
    enter_object("dimensions") %>%
    gather_array() %>%
    append_values_string("dimensions")
  
  headers_dims <- headers_dims[,setdiff(names(headers_dims), 
                                        c("document.id","array.index")), drop = FALSE]
  
  
  data_json <- tidy_json %>% enter_object("data")
  
  data_meta <- data_json %>% 
    spread_values(isDataGolden = jlogical("isDataGolden"),
                  rows = jnumber("rowCount"))
  
  data_totals <- data_json %>% 
    enter_object("totals") %>% 
    gather_array() %>% 
    enter_object("values") %>% 
    gather_array() %>% 
    append_values_number("totals")
  
  data_min <- data_json %>% 
    enter_object("minimums") %>% 
    gather_array() %>% 
    enter_object("values") %>% 
    gather_array() %>% 
    append_values_number("minimums")
  
  data_max <- data_json %>% 
    enter_object("maximums") %>% 
    gather_array() %>% 
    enter_object("values") %>% 
    gather_array() %>% 
    append_values_number("maximums")
  
  data_json %>% enter_object("rows") %>% 
    gather_array() %>% 
    enter_object("dimensions") %>% 
    gather_array() %>% 
    append_values_string()

}

#' ga v4 data parsing
#'
#' x is response_list$reports[[1]] from google_analytics_4_parse_batch
#' @keywords internal
google_analytics_4_parse <- function(x){
  
  message("Parsing GA API v4")
 
  #### x <- ga_data2$reports[[1]]
  
  columnHeader <- x$columnHeader
  data <- x$data$rows
  hasDateComparison <- if(length(data[[1]]$metrics) == 2) TRUE else FALSE
  
  if(!is.null(data$isDataGolden)){
    if(!data$isDataGolden)
      warning("Data is not Golden - may change on subsequent API calls.")
  }
  
  if(!is.null(data$filteredForPrivacyReasons)){
    warning("Some data has been filtered for privacy reasons.")
  }
  
  if(!is.null(data$samplesReadCounts)){
    warning("Data is sampled.")
  }
  
  dim_names <- unlist(columnHeader$dimensions)
  met_names <- unlist(lapply(columnHeader$metricHeader$metricHeaderEntries, function(x) x$name))
  met_names1 <- gsub("ga:","",met_names)
  # met_types <- unlist(lapply(columnHeader$metricHeader$metricHeaderEntries, function(x) x$type))
  
  if(is.null(data)){
    message("No data found")
    return(NULL)
  }
  
  dims <- matrix(unlist(lapply(data, function(x) x$dimensions)),
                 ncol = length(dim_names), byrow = TRUE)
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
  out <- data.frame(dims, mets,
                    stringsAsFactors = FALSE, row.names = 1:nrow(mets))
  
  out_names <- c(dim_names, met_names)
  out_names <- gsub("ga:","",out_names)
  
  names(out) <- make.names(out_names, unique=TRUE)
  
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
  
  samplePercent <-  100
  if(!is.null(x$data$samplesReadCounts)){
      samplePercent <- round(100 * (as.numeric(x$data$samplesReadCounts) / as.numeric(x$data$samplingSpaceSizes)), 2)
      message("Data is sampled, based on ", samplePercent, "% of visits." )
  }
  
  testthat::expect_s3_class(out, "data.frame")
  
  out
  
}


#' New parse GA account summary
#' 
#' @param x The account summary items
#' @import tidyjson
parse_ga_account_summary <- function(x){
  
  json_accounts <- jsonlite::toJSON(x$items)
  class(json_accounts) <- c(class(json_accounts), "character")
  tidy_json <- json_accounts %>% as.tbl_json()
  
  tidy_json <- tidy_json %>% 
    gather_array() %>% 
    spread_values(accountId = jstring("id"), 
                  accountName = jstring("name")) %>%
    enter_object("webProperties") %>%
    gather_array() %>%
    spread_values(webPropertyId = jstring("id"), 
                  webPropertyName = jstring("name"),
                  internalWebPropertyId = jstring("internalWebPropertyId"),
                  level = jstring("level"),
                  websiteUrl = jstring("websiteUrl")) %>%
    enter_object("profiles") %>%
    gather_array() %>%
    spread_values(viewId = jstring("id"), 
                  viewName = jstring("name"),
                  type = jstring("type"),
                  starred = jstring("starred"))
  
  ## remove tidyjson artifacts
  out <- tidy_json[,setdiff(names(tidy_json), c("document.id","array.index"))]
  
  out
}



parse_google_analytics <- function(x){

  message("Request to profileId: ", x$profileInfo$profileId,
          #     " accountId: ", x$profileInfo$accountId,
          #     " webPropertyId: ", x$profileInfo$webPropertyId,
          " (", x$profileInfo$profileName, ")")


  samplePercent <-  100
  if(!is.null(x$containsSampledData)){
    if(x$containsSampledData) {
      samplePercent <- round(100 * (as.numeric(x$sampleSize) / as.numeric(x$sampleSpace)), 2)
      message("Data is sampled, based on ", samplePercent, "% of visits. Use samplingLevel='WALK' to mitigate it." )
    }
  }

  if(x$kind == "analytics#gaData"){
    gadata <- parse_google_analytics_ga(x)
  } else if(x$kind == "analytics#mcfData"){
    gadata <- parse_google_analytics_mcf(x)
  }

  message("Fetched: ",
          paste(colnames(gadata), collapse = " "),
          ". [", NROW(gadata), "] total results out of a possible [", x$totalResults, "], Start-Index: ", x$query$`start-index`)

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

  if ("MCF_SEQUENCE" %in% types) {
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
    data_df <- as.data.frame(do.call(rbind, lapply(x$rows, unlist)), stringsAsFactors = FALSE)
    colnames(data_df) <- names
  }
  return(data_df)

}

parse_google_analytics_meta <- function(x){

  dim_mets <- x$items$id

  dim_mets_attr <- x$items$attributes

  data.frame(name=dim_mets, dim_mets_attr, stringsAsFactors = F)

}