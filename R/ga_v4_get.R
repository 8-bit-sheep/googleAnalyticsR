  #' Google Analytics v4 API fetch
#'
#' @param viewId viewId of data to get.
#' @param date_range character or date vector of format c(start, end). Optional c(s1,e1,s2,e2)
#' @param metrics Metric to fetch. Required. Supports calculated metrics.
#' @param dimensions Dimensions to fetch.
#' @param filters A list of dimensionFilterClauses or metricFilterClauses
#' @param segments Segments of the data
#' @param pivots Pivots of the data
#' @param cohorts Cohorts created by \link{makeCohortGroup}
#' @param pageToken Where to start the data fetch,
#' @param pageSize How many rows to fetch. Max 10000 each batch.
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
                          date_range=NULL,
                          metrics=NULL,
                          dimensions=NULL,
                          dim_filters=NULL,
                          met_filters=NULL,
                          filtersExpression=NULL,
                          orderBys=NULL,
                          segments=NULL,
                          pivots=NULL,
                          cohorts=NULL,
                          pageToken=1,
                          pageSize=1000,
                          samplingLevel=c("DEFAULT", "SMALL","LARGE"),
                          metricFormat=NULL,
                          histogramBuckets=NULL) {

  samplingLevel <- match.arg(samplingLevel)
  
  if(all(is.null(date_range), is.null(cohorts))){
    stop("Must supply one of date_range or cohorts")
  }
  
  if(!is.null(cohorts)){
    testthat::expect_true(cohort_metric_check(metrics))
    testthat::expect_true(cohort_dimension_check(dimensions))
    if(!is.null(date_range)){
      stop("Don't supply date_range when using cohorts")
    }
  }
  
  if(is.null(metrics)){
    stop("Must supply a metric")
  }

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
        cohortGroup=cohorts,
        pageToken=as.character(pageToken),
        pageSize = pageSize,
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
                               date_range=NULL,
                               metrics=NULL,
                               dimensions=NULL,
                               date_range_two=NULL,
                               dim_filters=NULL,
                               met_filters=NULL,
                               filtersExpression=NULL,
                               segments=NULL,
                               pivots=NULL,
                               cohorts=NULL,
                               max=1000,
                               samplingLevel=c("DEFAULT", "SMALL","LARGE"),
                               metricFormat=NULL,
                               histogramBuckets=NULL){

  ## for same Id and daterange, v4 batches can be made
  if(max > 10000){
    ## how many v4 batch requests can be made at one time (5-1) due to bad maths
    batchLimit <- 5
    meta_batch_start_index <- seq(from=1, to=max, by=10000*batchLimit)
    batches <- length(meta_batch_start_index)
    
    message("V4 Batching data into [", batches, "] calls.")

    ## loop for each over 50000
    requests <- lapply(meta_batch_start_index, function(meta){
      
      message("Outer batch loop:", meta)
      batch_start_index <- seq(from=meta, to=min(meta+((batchLimit-1)*10000),max), 10000)
      
      ## loop for each 10000 fetch
      requests_in <- lapply(batch_start_index, function(x){
        message("Inner batch loop:", x)
        req <- make_ga_4_req(viewId=viewId,
                             date_range=date_range,
                             metrics=metrics,
                             dimensions=dimensions,
                             dim_filters=dim_filters,
                             met_filters=met_filters,
                             filtersExpression = filtersExpression,
                             segments=segments,
                             pivots=pivots,
                             cohorts=cohorts,
                             pageToken = x,
                             pageSize = 10000,
                             samplingLevel=samplingLevel,
                             metricFormat=metricFormat,
                             histogramBuckets=histogramBuckets)
        
      })
      

      ## a list of gav4 results
      inner_reqs <- fetch_google_analytics_4(requests_in)
      
      ## make it one dataframe
      if(inherits(inner_reqs, "list")){
        out <- Reduce(rbind, inner_reqs)
      } else if(inherits(inner_reqs, "data.frame")){
        out <- inner_reqs
      } else {
        stop("Output class not recognised: ", class(inner_reqs))
      }
      
      out
      
    })

    ## a list of dataframes, to one
    out <- Reduce(rbind, requests)
    
  } else {
    ## normal under 10000
    req <- make_ga_4_req(viewId=viewId,
                         date_range=date_range,
                         metrics=metrics,
                         dimensions=dimensions,
                         dim_filters=dim_filters,
                         met_filters=met_filters,
                         filtersExpression = filtersExpression,
                         segments=segments,
                         pivots=pivots,
                         cohorts=cohorts,
                         pageToken = 1,
                         pageSize = max,
                         samplingLevel=samplingLevel,
                         metricFormat=metricFormat,
                         histogramBuckets=histogramBuckets)
    out <- fetch_google_analytics_4(req)
  }
  
  out


}

#' Fetch multiple GAv4 requests
#'
#' @param requests A list of GAv4 requests created by make_ga_4_req
#'
#' @return dataframe of GA results
#'
#' @importFrom googleAuthR gar_api_generator
#' @export
fetch_google_analytics_4 <- function(request_list){

  testthat::expect_type(request_list, "list")

  body <- list(
    reportRequests = request_list
  )

  f <- gar_api_generator("https://analyticsreporting.googleapis.com/v4/reports:batchGet",
                         "POST",
                         data_parse_function = google_analytics_4_parse_batch,
                         # data_parse_function = function(x) x,
                         simplifyVector = FALSE)

  message("Fetching Google Analytics v4 API data")

  out <- f(the_body = body)
  
  ## if only one entry in the list, return the dataframe
  if(length(out) == 1) out <- out[[1]]

  out
}

