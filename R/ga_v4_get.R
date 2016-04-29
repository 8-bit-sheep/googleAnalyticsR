##docs: https://developers.google.com/analytics/trusted-testing/reporting/
## https://developers.google.com/analytics/trusted-testing/reporting/rest/v4/reports/batchGet#Dimension

## TODO:
##       segments

#' Google Analytics v4 API fetch
#'
#' @param viewId viewId of data to get.
#' @param date_range character or date vector of format c(start, end). Optionc("#752324", "white", "white", "indianred")c("#FFFFFF")al c(s1,e1,s2,e2)
#' @param metrics Metric to fetch. Required. Supports calculated metrics.
#' @param dimensions Dimensions to fetch.
#' @param filters A list of dimensionFilterClauses or metricFilterClauses
#' @param segments Segments of the data
#' @param pivots Pivots of the data
#' @param samplingLevel Sample level
#' @param metricFormat If supplying calculated metrics, specify the metric type
#'
#' @description
#'   This function constructs the Google Analytics API v4 call to be called
#'   via \code{fetch_google_analytics_4}
#'
#'
#' @section Metrics:
#'   Metrics support calculated metrics like ga:users / ga:sessions if you supply
#'   them in a named vector.
#'
#'   You must supply the correct 'ga:' prefix unlike normal metrics
#'
#'   You can mix calculated and normal metrics like so:
#'
#'   \code{customMetric <- c(sessionPerVisitor = "ga:sessions / ga:visitors",
#'                           "bounceRate",
#'                           "entrances")}
#'
#'    You can also optionally supply a \code{metricFormat} parameter that must be
#'    the same length as the metrics.  \code{metricFormat} can be:
#'    \code{METRIC_TYPE_UNSPECIFIED, INTEGER, FLOAT, CURRENCY, PERCENT, TIME}
#'
#'    All metrics are currently parsed to as.numeric when in R.
#'
#' @section Dimensions:
#'
#'   Supply a character vector of dimensions, with or without \code{ga:} prefix.
#'
#'   Optionally for numeric dimension types such as
#'   \code{ga:hour, ga:browserVersion, ga:sessionsToTransaction}, etc. supply
#'   histogram buckets suitable for histogram plots.
#'
#'   If non-empty, we place dimension values into buckets after string to int64.
#'   Dimension values that are not the string representation of an integral value
#'   will be converted to zero. The bucket values have to be in increasing order.
#'   Each bucket is closed on the lower end, and open on the upper end.
#'   The "first" bucket includes all values less than the first boundary,
#'   the "last" bucket includes all values up to infinity.
#'   Dimension values that fall in a bucket get transformed to a new dimension
#'   value. For example, if one gives a list of "0, 1, 3, 4, 7", then we
#'   return the following buckets: -
#'
#'   bucket #1: values < 0, dimension value "<0"
#'   bucket #2: values in [0,1), dimension value "0"
#'   bucket #3: values in [1,3), dimension value "1-2"
#'   bucket #4: values in [3,4), dimension value "3"
#'   bucket #5: values in [4,7), dimension value "4-6"
#'   bucket #6: values >= 7, dimension value "7+"
#'
#' @export
make_ga_4_req <- function(viewId,
                          date_range,
                          metrics,
                          dimensions=NULL,
                          dim_filters=NULL,
                          met_filters=NULL,
                          filtersExpression=NULL,
                          orderBys=NULL,
                          segments=NULL,
                          pivots=NULL,
                          max=1000,
                          samplingLevel=c("DEFAULT", "SMALL","LARGE"),
                          metricFormat=NULL,
                          histogramBuckets=NULL) {

  samplingLevel <- match.arg(samplingLevel)

  id <- sapply(viewId, checkPrefix, prefix = "ga")

  date_list_one <- date_ga4(date_range[1:2])
  if(length(date_range) == 4){
    date_list_two <- date_ga4(date_range[3:4])
  } else {
    date_list_two <- NULL
  }

  dim_list <- dimension_ga4(dimensions, histogramBuckets)
  met_list <- metric_ga4(metrics, metricFormat)

  # order the dimensions if histograms
  if(all(is.null(orderBys), !is.null(histogramBuckets))){
    bys <- intersect(dimensions, names(histogramBuckets))
    orderBys <- lapply(bys,
                       order_type,
                       FALSE,
                       "HISTOGRAM_BUCKET")
  }


  request <-
    structure(
      list(
        viewId = id,
        dateRanges = list(
          date_list_one,
          date_list_two
        ),
        samplingLevel = samplingLevel,
        dimensions = dim_list,
        metrics = met_list,
        dimensionFilterClauses = dim_filters,
        metricFilterClauses = met_filters,
        filtersExpression = filtersExpression,
        orderBys = orderBys,
        segments = segments,
        pivots = pivots,
        pageSize = max,
        includeEmptyRows = TRUE
      ),
      class = "ga4_req")


  request <- rmNullObs(request)

}



#' Do a single request
#'
#' @inheritParams make_ga_4_req
#' @export
google_analytics_4 <- function(viewId,
                               date_range,
                               metrics,
                               dimensions=NULL,
                               date_range_two=NULL,
                               dim_filters=NULL,
                               met_filters=NULL,
                               filtersExpression=NULL,
                               segments=NULL,
                               pivots=NULL,
                               max=1000,
                               samplingLevel=c("DEFAULT", "SMALL","LARGE"),
                               metricFormat=NULL,
                               histogramBuckets=NULL){

  req <- make_ga_4_req(viewId=viewId,
                       date_range=date_range,
                       metrics=metrics,
                       dimensions=dimensions,
                       dim_filters=dim_filters,
                       met_filters=met_filters,
                       filtersExpression = filtersExpression,
                       segments=segments,
                       pivots=pivots,
                       max=max,
                       samplingLevel=samplingLevel,
                       metricFormat=metricFormat,
                       histogramBuckets=histogramBuckets)

  fetch_google_analytics_4(req)
}

#' Fetch multiple GAv4 requests
#'
#' @param requests A list of GAv4 requests created by make_ga_4_req
#'
#' @return dataframe of GA results
#'
#' @export
fetch_google_analytics_4 <- function(request_list){

  stopifnot(inherits(request_list, "list"))

  raw <- getOption("googleAnalyticsR.raw_req")

  if(raw) {
    warning("No data parsing due to 'getOption('googleAnalyticsR.raw_req')' set to TRUE")
    dpf <- function(x) x
    } else {
      dpf <- google_analytics_4_parse_batch
    }

  ga_auth()

  body <- list(
    reportRequests = request_list
  )

  f <- googleAuthR::gar_api_generator("https://analytics.googleapis.com/v4/reports:batchGet",
                                      "POST",
                                      data_parse_function = dpf)

  message("Fetching Google Analytics v4 API data")

  out <- f(the_body = body)

  attr(out, "dates") <- request_list$dateRanges

  out
}


#' ga_auth
#'
#' @param token If you have an existing token already
#' @param new_user Go through the auth flow again
#' @param verbose More feedback
#'
#' @export
ga_auth <- function(token = NULL,
                    new_user = FALSE,
                    verbose = TRUE){
  options("googleAuthR.client_id" = "858905045851-3beqpmsufml9d7v5d1pr74m9lnbueak2.apps.googleusercontent.com")
  options("googleAuthR.client_secret" = "bnmF6C-ScpSR68knbGrHBQrS")
  options("googleAuthR.jsonlite.simplifyVector" = FALSE)
  options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics", "https://www.googleapis.com/auth/analytics.readonly"))
  options("googleAnalyticsR.raw_req" = FALSE)

  googleAuthR::gar_auth(token = token,
                        new_user = new_user)

}

#' ga v4 parse batching
#'
google_analytics_4_parse_batch <- function(response_list){

  parsed <- lapply(response_list$reports, google_analytics_4_parse)

  ## if only one entry in the list, return the dataframe
  if(length(parsed) == 1) parsed <- parsed[[1]]

  parsed

}
#' ga v4 data parsing
#'
#' x is response_list$reports[[1]] from google_analytics_4_parse_batch
#' @keywords internal
google_analytics_4_parse <- function(x){

  message("Parsing GA API v4")

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
  # met_types <- unlist(lapply(columnHeader$metricHeader$metricHeaderEntries, function(x) x$type))



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

  attr(out, "totals") <- x$data$totals
  attr(out, "minimums") <- x$data$minimums
  attr(out, "maximums") <- x$data$maximums
  attr(out, "isDataGolden") <- x$data$isDataGolden
  attr(out, "rowCount") <- x$data$rowCount

  out

}

#' pivot data parsing
#'
#' @keywords internal
#' @family pivot functions
pivot_ga4_parse <- function(x, hasDateComparison){
  if(is.null(x$columnHeader$metricHeader$pivotHeaders)) {
    return(NULL)
    }

  message("Adding pivot data")

  pivotHeaders <- x$columnHeader$metricHeader$pivotHeaders
  pivot_col_names <- makePivotNames(pivotHeaders)

  pivotDataRows <- x$data$rows

  pivotData1 <- makePivotData(pivotDataRows, pivot_col_names)

  if(hasDateComparison) {
    pivotData2 <- makePivotData(pivotDataRows, pivot_col_names, index=2)
    out <- cbind(pivotData1, pivotData2)
  } else {
    out <- pivotData1
  }

  out

}

#' Make pivot data rows
#'
#' @param pivotRows from x$data$rows
#' @param index 1 for normal data, 2 for date comparison
#'
#' @keywords internal
#' @family pivot functions
makePivotData <- function(pivotRows, pivotNames, index=1){

  if(index==2) pivotNames <- paste0(pivotNames, ".d2")

  pivotData <- lapply(pivotRows, function(row) row$metrics[[index]]$pivotValues)
  pivotData <- lapply(lapply(pivotData, function(x) lapply(x, unlist)), unlist)

  pivotData <- Reduce(rbind, pivotData)
  pivotData <- matrix(as.numeric(pivotData), ncol=length(pivotNames))
  colnames(pivotData) <- pivotNames

  # pivotData

  data.frame(pivotData,
             row.names = 1:(nrow(pivotData)),
             stringsAsFactors = FALSE)
}

#' Make pivot column names
#'
#' @keywords internal
#' @family pivot functions
makePivotNames <- function(pivotHeaders){
  n <- lapply(pivotHeaders,
         ## loop over number of pivot columns expected
         function(pivots)
           Reduce(c,
                  lapply(pivots$pivotHeaderEntries,
                         ## alternating per metric
                         function(pivotCol)
                           gsub("ga:","",
                                paste(paste(pivotCol$dimensionNames,
                                            pivotCol$dimensionValues,
                                            sep=".", collapse = "_"),
                                      pivotCol$metric$name, sep=".")
                           )
                  )
           )
  )

  pivNames <- lapply(n, make.names, unique = TRUE)

  unlist(pivNames)
}

#' Make a segment object for use
#'
#' @param name The name of the segment for the reports.
#' @param segment_filters A vector of \code{segment_filter}.
#' @param exclude Should this match or exclude the segment defined.
#' @param type Simple or Sequence segment.
#' @param firstStepMatch If a sequence segment, should first step match first hit.
#'
#' @return a segmentFilter object. You can pass a list of these to the request.
#' @family v4 segment functions
#' @export
segment_ga4 <- function(name,
                        segment_filters,
                        exclude=FALSE,
                        type = c("simple","sequence"),
                        firstStepMatch=FALSE){
  type <- match.arg(type)

  segment_filters <- as.list(unname(segment_filters))

  simple <- NULL
  sequence_seg <- NULL

  if(type == "simple"){
    simple <- simpleSegment(segment_filters)
  } else {
    sequence_seg <- sequenceSegment(segment_filters, firstStepMatch)
  }

  segmentFilter(matchComplement = exclude,
                simpleSegment = simple,
                sequenceSegment = sequence_seg)

}

#' Make a segment filter
#'
#' @param matchType If to be used in sequence segment,
#'   should it precede or immmediately precede
#'
#' @return An orFiltersForSegment object for use in segment_ga4
#' @family v4 segment functions
#' @export
segment_filter <- function(type = c("metric", "dimensions"),
                           matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){




  # orFiltersForSegment(..list of segmentfilterclauses...)

}
