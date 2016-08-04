#' Google Analytics v4 API fetch
#' 
#' @description
#'   This function constructs the Google Analytics API v4 call to be called
#'   via \link{fetch_google_analytics_4}
#'
#' @param viewId viewId of data to get.
#' @param date_range character or date vector of format \code{c(start, end)} or 
#'   for two date ranges: \code{c(start1,end1,start2,end2)}
#' @param metrics Metric to fetch. Required. Supports calculated metrics.
#' @param dimensions Dimensions to fetch.
#' @param dim_filters A \link{filter_clause_ga4} wrapping \link{dim_filter}
#' @param met_filters A \link{filter_clause_ga4} wrapping \link{met_filter}
#' @param filtersExpression A v3 API style simple filter string. Not used with other filters. 
#' @param order An \link{order_type} object
#' @param segments List of segments as created by \link{segment_ga4}
#' @param pivots Pivots of the data as created by \link{pivot_ga4}
#' @param cohorts Cohorts created by \link{make_cohort_group}
#' @param pageToken Where to start the data fetch
#' @param pageSize How many rows to fetch. Max 10000 each batch.
#' @param samplingLevel Sample level
#' @param metricFormat If supplying calculated metrics, specify the metric type
#' @param histogramBuckets For numeric dimensions such as hour, a list of buckets of data.
#'   See details in \link{make_ga_4_req}
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
#' \itemize{
#'   \item bucket #1: values < 0, dimension value "<0"
#'   \item bucket #2: values in [0,1), dimension value "0"
#'   \item bucket #3: values in [1,3), dimension value "1-2"
#'   \item bucket #4: values in [3,4), dimension value "3"
#'   \item bucket #5: values in [4,7), dimension value "4-6"
#'   \item bucket #6: values >= 7, dimension value "7+"
#'  }
#'  
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' 
#' ## authenticate, 
#' ## or use the RStudio Addin "Google API Auth" with analytics scopes set
#' ga_auth()
#' 
#' ## get your accounts
#' account_list <- google_analytics_account_list()
#' 
#' ## pick a profile with data to query
#' 
#' ga_id <- account_list[23,'viewId']
#' 
#' ga_req1 <- make_ga_4_req(ga_id, 
#'                          date_range = c("2015-07-30","2015-10-01"),
#'                          dimensions=c('source','medium'), 
#'                          metrics = c('sessions'))
#' 
#' ga_req2 <- make_ga_4_req(ga_id, 
#'                          date_range = c("2015-07-30","2015-10-01"),
#'                          dimensions=c('source','medium'), 
#'                          metrics = c('users'))
#'                          
#' fetch_google_analytics_4(list(ga_req1, ga_req2))
#' 
#' }
#' 
#' 
#' @family GAv4 fetch functions
#' @export
make_ga_4_req <- function(viewId,
                          date_range=NULL,
                          metrics=NULL,
                          dimensions=NULL,
                          dim_filters=NULL,
                          met_filters=NULL,
                          filtersExpression=NULL,
                          order=NULL,
                          segments=NULL,
                          pivots=NULL,
                          cohorts=NULL,
                          pageToken=0,
                          pageSize=1000,
                          samplingLevel=c("DEFAULT", "SMALL","LARGE"),
                          metricFormat=NULL,
                          histogramBuckets=NULL) {

  # message("Fetching: ", viewId, " pageToken: ", pageToken, " pageSize: ", pageSize)
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
  if(all(is.null(order), !is.null(histogramBuckets))){
    bys <- intersect(dimensions, names(histogramBuckets))
    order <- lapply(bys,
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
        orderBys = order,
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



#' GAv4 single request
#'
#' A convenience function that wraps \link{make_ga_4_req} and \link{fetch_google_analytics_4}
#'  for the common case of one GA data request.
#'  
#' Will perform batching if over the 10000 row per API call limit.
#' 
#' @inheritParams make_ga_4_req
#' @param max Maximum number of rows to fetch. Defaults at 1000.
#' 
#' @return A Google Analytics data.frame
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' 
#' ## authenticate, 
#' ## or use the RStudio Addin "Google API Auth" with analytics scopes set
#' ga_auth()
#' 
#' ## get your accounts
#' account_list <- google_analytics_account_list()
#' 
#' ## pick a profile with data to query
#' 
#' ga_id <- account_list[23,'viewId']
#' 
#' ga_data <- google_analytics_4(ga_id, 
#'                               date_range = c("2015-07-30","2015-10-01"),
#'                               dimensions=c('source','medium'), 
#'                               metrics = c('sessions','bounces'))
#' 
#' }
#' 
#' @family GAv4 fetch functions
#' @export
google_analytics_4 <- function(viewId,
                               date_range=NULL,
                               metrics=NULL,
                               dimensions=NULL,
                               dim_filters=NULL,
                               met_filters=NULL,
                               filtersExpression=NULL,
                               order=NULL,
                               segments=NULL,
                               pivots=NULL,
                               cohorts=NULL,
                               max=1000,
                               samplingLevel=c("DEFAULT", "SMALL","LARGE"),
                               metricFormat=NULL,
                               histogramBuckets=NULL){

  max <- as.integer(max)
  ## for same Id and daterange, v4 batches can be made
  reqRowLimit <- 10000
  
  if(max > reqRowLimit){
    message("Batching requests as max over ", reqRowLimit)
    ## how many v4 batch requests can be made at one time
  }
  meta_batch_start_index <- seq(from=0, to=max, by=reqRowLimit)
  # batches <- length(meta_batch_start_index)
  
  ## make a list of the requests
  requests <- lapply(meta_batch_start_index, function(start_index){
    start_index <- as.integer(start_index)
    remaining <- max - start_index
    
    out <- make_ga_4_req(viewId=viewId,
                         date_range=date_range,
                         metrics=metrics,
                         dimensions=dimensions,
                         dim_filters=dim_filters,
                         met_filters=met_filters,
                         filtersExpression = filtersExpression,
                         order=order,
                         segments=segments,
                         pivots=pivots,
                         cohorts=cohorts,
                         pageToken = start_index,
                         pageSize = min(remaining, reqRowLimit),
                         samplingLevel=samplingLevel,
                         metricFormat=metricFormat,
                         histogramBuckets=histogramBuckets)
    
    })
    
  fetch_google_analytics_4(requests, merge = TRUE)

}

#' Fetch multiple GAv4 requests
#' 
#' Fetch the GAv4 requests as created by \link{make_ga_4_req}
#'
#' @param request_list A list of requests created by \link{make_ga_4_req}
#' @param merge If TRUE then will rbind that list of data.frames
#'
#' @return A dataframe if one request, or a list of data.frames if multiple.
#'
#' @importFrom googleAuthR gar_api_generator
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' 
#' ## authenticate, 
#' ## or use the RStudio Addin "Google API Auth" with analytics scopes set
#' ga_auth()
#' 
#' ## get your accounts
#' account_list <- google_analytics_account_list()
#' 
#' ## pick a profile with data to query
#' 
#' ga_id <- account_list[23,'viewId']
#' 
#' ga_req1 <- make_ga_4_req(ga_id, 
#'                          date_range = c("2015-07-30","2015-10-01"),
#'                          dimensions=c('source','medium'), 
#'                          metrics = c('sessions'))
#' 
#' ga_req2 <- make_ga_4_req(ga_id, 
#'                          date_range = c("2015-07-30","2015-10-01"),
#'                          dimensions=c('source','medium'), 
#'                          metrics = c('users'))
#'                          
#' fetch_google_analytics_4(list(ga_req1, ga_req2))
#' 
#' }
#' 
#' @family GAv4 fetch functions
#' @export
fetch_google_analytics_4 <- function(request_list, merge = FALSE){

  testthat::expect_type(request_list, "list")

  if(!is.null(request_list$viewId) || length(request_list) <= 5){
    
    request_list <- unitToList(request_list)
    
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
    if(length(out) == 1){
      out <- out[[1]]
    } else {
      ## returned a list of data.frames
      if(merge){
       
        if(length(unique((lapply(out, function(x) names(x))))) != 1){
         stop("List of dataframes have non-identical column names")
        }
        
        out <- Reduce(rbind, out)
        
      }
    }
    
  } else {
    ## do gar_batching
    stop("Too many requests to V4 batch, must be 5 or under.")
  }


  out
}

