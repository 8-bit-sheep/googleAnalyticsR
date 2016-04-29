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