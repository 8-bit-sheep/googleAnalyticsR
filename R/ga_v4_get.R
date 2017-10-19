#' Make a Google Analytics v4 API fetch
#' 
#' @description
#'   This function constructs the Google Analytics API v4 call to be called
#'   via \link{fetch_google_analytics_4}
#'
#' @param viewId viewId of data to get.
#' @param date_range character or date vector of format \code{c(start, end)} or 
#'   for two date ranges: \code{c(start1,end1,start2,end2)}
#' @param metrics Metric to fetch. Supports calculated metrics.
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

  samplingLevel <- match.arg(samplingLevel)
  
  if(all(is.null(date_range), is.null(cohorts))){
    stop("Must supply one of date_range or cohorts")
  }
  
  if(!is.null(cohorts)){
    assertthat::assert_that(cohort_metric_check(metrics),
                            cohort_dimension_check(dimensions))
    if(!is.null(date_range)){
      warning("Don't supply date_range when using cohorts, setting date_range to NULL")
      date_range <- NULL
    }
  }
  
  if(is.null(metrics)){
    stop("Must supply a metric")
  }
  
  if(!is.null(segments)){
    if(!any("segment" %in% dimensions)){
      dimensions <- c(dimensions, "segment")
    }
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



#' Get Google Analytics v4 data (single request)
#'
#' A convenience function that wraps \link{make_ga_4_req} and \link{fetch_google_analytics_4}
#'  for the common case of one GA data request.
#'  
#' Will perform automatic batching if over the 10000 row per API call limit.
#' 
#' @section Anti-sampling:
#' 
#' \code{anti_sample} being TRUE ignores \code{max} as the API call is split over days 
#'   to mitigate the sampling session limit, in which case a row limit won't work.  Take the top rows
#'   of the result yourself instead e.g. \code{head(ga_data_unsampled, 50300)}
#' 
#' If you are lucky enough to need sub-day sampling, it will attempt to fetch per hour, but you are
#'   restricted to not using \code{dim_filter} argument if this is the case.  
#'   Try using \code{filtersExpression} instead.
#'   
#' \code{anti_sample} being TRUE will also set \code{samplingLevel='LARGE'} to minimise 
#'   the number of calls.
#' 
#' @inheritParams make_ga_4_req
#' @param max Maximum number of rows to fetch. Defaults at 1000. Use -1 to fetch all results. Ignored when \code{anti_sample=TRUE}.
#' @param anti_sample If TRUE will split up the call to avoid sampling.
#' @param anti_sample_batches "auto" default, or set to number of days per batch. 1 = daily.
#' @param slow_fetch For large results this slows down the API requests to avoid 500 errors
#' 
#' @return A Google Analytics data.frame
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' 
#' ## authenticate, or use the RStudio Addin "Google API Auth" with analytics scopes set
#' 
#' ga_auth()
#' 
#' ## get your accounts
#' 
#' account_list <- google_analytics_account_list()
#' 
#' ## account_list will have a column called "viewId"
#' account_list$viewId
#' 
#' ## View account_list and pick the viewId you want to extract data from
#' ga_id <- 123456
#' 
#' ## simple query to test connection
#' google_analytics_4(ga_id, 
#'                    date_range = c("2017-01-01", "2017-03-01"), 
#'                    metrics = "sessions", 
#'                    dimensions = "date")
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
                               histogramBuckets=NULL,
                               anti_sample = FALSE,
                               anti_sample_batches = "auto",
                               slow_fetch = FALSE){
  
  # cache_call(cache)
  
  max         <- as.integer(max)
  allResults  <- FALSE
  if(max < 0){
    ## size of 1 v4 batch 0 indexed
    max <- as.integer(49999)
    allResults <- TRUE
  }
  reqRowLimit <- as.integer(10000)
  
  if(!is.null(cohorts)){
    anti_sample <- FALSE
  }
  
  if(anti_sample){
    myMessage("anti_sample set to TRUE. Mitigating sampling via multiple API calls.", level = 3)
    return(anti_sample(viewId            = viewId,
                       date_range        = date_range,
                       metrics           = metrics,
                       dimensions        = dimensions,
                       dim_filters       = dim_filters,
                       met_filters       = met_filters,
                       filtersExpression = filtersExpression,
                       order             = order,
                       segments          = segments,
                       pivots            = pivots,
                       cohorts           = cohorts,
                       metricFormat      = metricFormat,
                       histogramBuckets  = histogramBuckets,
                       anti_sample_batches = anti_sample_batches,
                       slow_fetch = slow_fetch))
  }
  
  if(max > reqRowLimit){
    myMessage("Multi-call to API", level = 2)
  }
  
  meta_batch_start_index <- seq(from=0, to=max, by=reqRowLimit)
  
  ## make a list of the requests
  requests <- lapply(meta_batch_start_index, function(start_index){
    
    start_index <- as.integer(start_index)
    
    if(allResults){
      remaining <- as.integer(10000)
    } else {
      remaining   <- min(as.integer(max - start_index), reqRowLimit)
    }

    make_ga_4_req(viewId            = viewId,
                  date_range        = date_range,
                  metrics           = metrics,
                  dimensions        = dimensions,
                  dim_filters       = dim_filters,
                  met_filters       = met_filters,
                  filtersExpression = filtersExpression,
                  order             = order,
                  segments          = segments,
                  pivots            = pivots,
                  cohorts           = cohorts,
                  pageToken         = start_index,
                  pageSize          = remaining,
                  samplingLevel     = samplingLevel,
                  metricFormat      = metricFormat,
                  histogramBuckets  = histogramBuckets)
    
    })
  
  ## if non-batching, fetch one at a time
  if(slow_fetch){
    out <- fetch_google_analytics_4_slow(requests, max_rows = max, allRows = allResults)
    allResults <- FALSE
  } else {
    ## only gets up to 50000 first time as we don't know true total row count yet
    out <- fetch_google_analytics_4(requests, merge = TRUE)
  }

  ## if batching, get the rest of the results now we now precise rowCount
  if(allResults){
    all_rows <- as.integer(attr(out, "rowCount"))
    if(!is.null(out) && nrow(out) < all_rows){
      ## create the remaining requests
      meta_batch_start_index2 <- seq(from=50000, to=all_rows, by=reqRowLimit)
      ## make a list of the requests
      requests2 <- lapply(meta_batch_start_index2, function(start_index){
        
        start_index <- as.integer(start_index)
        remaining <- as.integer(10000)
        
        make_ga_4_req(viewId            = viewId,
                      date_range        = date_range,
                      metrics           = metrics,
                      dimensions        = dimensions,
                      dim_filters       = dim_filters,
                      met_filters       = met_filters,
                      filtersExpression = filtersExpression,
                      order             = order,
                      segments          = segments,
                      pivots            = pivots,
                      cohorts           = cohorts,
                      pageToken         = start_index,
                      pageSize          = remaining,
                      samplingLevel     = samplingLevel,
                      metricFormat      = metricFormat,
                      histogramBuckets  = histogramBuckets)
        
      })
      the_rest <- fetch_google_analytics_4(requests2, merge = TRUE)
      out <- rbind(out, the_rest)
      myMessage("All data downloaded, total of [",all_rows,"]", level = 3)
      
    } else {
      myMessage("One batch enough to get all results", level = 1)
    }
    
  }
  
  sampling_message(attr(out, "samplesReadCounts"), 
                   attr(out, "samplingSpaceSizes"), 
                   hasDateComparison = any(grepl("\\.d1|\\.d2", names(out))))
  
  out

}

#' Fetch GAv4 requests one at a time
#' 
#' Due to large complicated queries causing the v4 API to timeout, 
#'   this option is added to fetch via the more traditional one report per request
#' 
#' @param request_list A list of requests created by \link{make_ga_4_req}
#' @param max_rows Number of rows requested (if not fetched)
#' @param allRows Whether to fetch all available rows
#' 
#' @return A dataframe of all the requests
#' @importFrom googleAuthR gar_api_generator
#' @family GAv4 fetch functions
fetch_google_analytics_4_slow <- function(request_list, max_rows, allRows = FALSE){
  
  ## make the fetch function
  myMessage("Calling APIv4 slowly....", level = 2)
  ## make the function
  f <- gar_api_generator("https://analyticsreporting.googleapis.com/v4/reports:batchGet",
                         "POST",
                         data_parse_function = google_analytics_4_parse_batch,
                         # data_parse_function = function(x) x,
                         simplifyVector = FALSE)
  
  
  do_it <- TRUE
  
  ## just need the first one, which we modify
  the_req <- request_list[[1]]
  ## best guess at the moment
  actualRows <- max_rows
  
  response_list <- list()
  
  while(do_it){
    
    body <- list(
      reportRequests = the_req
    )
    
    myMessage("Slow fetch: [", 
              the_req$pageToken, "] from estimated actual Rows [", actualRows, "]", 
              level = 3)

    out <- try(f(the_body = body))
    
    error_check(out)
    
    actualRows <- attr(out[[1]], "rowCount")
    npt <- attr(out[[1]], "nextPageToken")
    
    if(is.null(npt)){
      do_it <- FALSE
    }
    
    the_req$pageToken <- npt
    response_list <- c(response_list, out)

  }

  Reduce(rbind, response_list)

}

#' Fetch multiple GAv4 requests
#' 
#' Fetch the GAv4 requests as created by \link{make_ga_4_req}
#' 
#' For same viewId, daterange, segments, samplingLevel and cohortGroup, v4 batches can be made
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

  assertthat::assert_that(is.list(request_list))
  ## amount of batches per v4 api call
  ga_batch_limit <- 5
  
  if(length(unique((lapply(request_list, function(x) x$viewId)))) != 1){
    stop("request_list must all have the same viewId")
  }
  
  if(length(unique((lapply(request_list, function(x) x$dateRanges)))) != 1){
    stop("request_list must all have the same dateRanges")
  }
  
  if(length(unique((lapply(request_list, function(x) x$segments)))) != 1){
    stop("request_list must all have the same segments")
  }
  
  if(length(unique((lapply(request_list, function(x) x$samplingLevel)))) != 1){
    stop("request_list must all have the same samplingLevel")
  }
  
  if(length(unique((lapply(request_list, function(x) x$cohortGroup)))) != 1){
    stop("request_list must all have the same cohortGroup")
  }
  
  myMessage("Calling APIv4....", level = 2)
  ## make the function
  f <- gar_api_generator("https://analyticsreporting.googleapis.com/v4/reports:batchGet",
                         "POST",
                         data_parse_function = google_analytics_4_parse_batch,
                         # data_parse_function = function(x) x,
                         simplifyVector = FALSE)
  
  ## if under 5, one call
  if(!is.null(request_list$viewId) || length(request_list) <= ga_batch_limit){
    myMessage("Single v4 batch", level = 2)
    request_list <- unitToList(request_list)
    
    body <- list(
      reportRequests = request_list
    )

    out <- try(f(the_body = body))
    
    out <- error_check(out)
    
  } else {

    myMessage("Multiple v4 batch", level = 2)
    ## get list of lists of ga_batch_limit
    request_list_index <- seq(1, length(request_list), ga_batch_limit)
    batch_list <- lapply(request_list_index, 
                         function(x) request_list[x:(x+(ga_batch_limit-1))])
    
    ## make the body for each v4 api call
    body_list <- lapply(batch_list, function(x) list(reportRequests = x))
    body_list <- rmNullObs(body_list)
      
    ## loop over the requests normally
    myMessage("Looping over maximum [", length(body_list), "] batches.", level = 1)
    response_list <- lapply(body_list, function(b){
      
      myMessage("Fetching data batch...", level = 3)
      
      out <- try(f(the_body = b))
      
      error_check(out)
      
    })
    
    out <- unlist(response_list, recursive = FALSE)
    
    
  }
  

  
  ## if only one entry in the list, return the dataframe
  if(length(out) == 1){
    out <- out[[1]]
  } else {
    ## returned a list of data.frames
    if(merge){

      ## if an empty list, return NULL
      if(all(vapply(out, is.null, logical(1)))){
        out <- NULL
      } else {
        ## check all dataframes have same columns
        df_names <- rmNullObs(lapply(out, function(x) names(x)))
        if(length(unique(df_names)) != 1){
          stop("List of dataframes have non-identical column names. Got ", 
               paste(lapply(out, function(x) names(x)), collapse = " "))
        }
        
        out <- Reduce(rbind, out)
      }

      
    }
  }

  myMessage("Downloaded [",NROW(out),"] rows from a total of [",attr(out, "rowCount"), "].", level = 3)

  out
}
