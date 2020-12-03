version_aw <- function(){
  "v1alpha"
}


#' Google Analytics Data for GA4 (App+Web)
#'
#' Fetches Google Analytics from the Data API for Google Analytics 4 (Previously App+Web)
#'
#' @seealso \href{https://developers.google.com/analytics/devguides/reporting/data/v1}{Google Documentation}
#'
#' @details
#'
#' @inheritParams Entity
#' @inheritParams RunReportRequest
#' @param date_range A vector of length two with start and end dates in YYYY-MM-DD format
#' @param dimensionDelimiter If combining dimensions in one column, the delimiter for the value field
#' @param realtime If TRUE then will call the real-time reports, that have a more limited set of dimensions/metrics - see \url{https://developers.google.com/analytics/devguides/reporting/data/v1/realtime-basics}
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family GA4 functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' # send up to 4 date ranges
#' multi_date <- ga_data(
#'   206670707,
#'   metrics = c("activeUsers","sessions"),
#'   dimensions = c("date","city","dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27", "2020-04-30", "2020-05-27"),
#'   dimensionFilter = ga_data_filter("city"=="Copenhagen"),
#'   limit = 100
#'   )
#' 
#' 
#' # metric and dimension expressions
#' 
#' # create your own named metrics
#' met_expression <- ga_data(
#'   206670707,
#'   metrics = c("activeUsers","sessions",sessionsPerUser = "sessions/activeUsers"),
#'   dimensions = c("date","city","dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27"),
#'   limit = 100
#'   )
#'
#' # create your own aggregation dimensions
#' dim_expression <- ga_data(
#'   206670707,
#'   metrics = c("activeUsers","sessions"),
#'   dimensions = c("date","city","dayOfWeek", cdow = "city/dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27"),
#'   limit = 100
#'   )
#'   
#' # run a real-time report (no date dimension allowed)
#' realtime <- ga_data(
#'   206670707,
#'   metrics = "activeUsers",
#'   dimensions = c("city","unifiedScreenName"),
#'   limit = 100,
#'   realtime = TRUE)
#' 
#' }
ga_data <- function(propertyId,
                    metrics,
                    date_range = NULL,
                    dimensions = NULL,
                    dimensionFilter = NULL,
                    dimensionDelimiter = "/",
                    metricFilter = NULL,
                    # metricAggregations = NULL,
                    orderBys = NULL,
                    limit = 100,
                    realtime=FALSE) {
  
  # in case someone passes in a filter instead of an expression
  dimensionFilter <- as_filterExpression(dimensionFilter)
  metricFilter <- as_filterExpression(metricFilter)
 
  
  # if(!is.null(metricAggregations)){
  #   assert_that(all(metricAggregations %in% c("TOTAL",
  #                                              "MINIMUM",
  #                                              "MAXIMUM",
  #                                              "COUNT")))
  # }
  #TODO
  metricAggregations <- NULL
  
  dims <- gaw_dimension(dimensions, delimiter = dimensionDelimiter)
  mets <- gaw_metric(metrics)
  
  if(realtime){
    
    brrr <- RunRealtimeReport(
      dimensions = dims,
      metrics = mets,
      limit = limit,
      dimensionFilter = dimensionFilter,
      metricFilter = metricFilter,
      metricAggregations = metricAggregations,
      orderBys = orderBys,
      returnPropertyQuota = TRUE
    )
    
    myMessage("Realtime Report Request", level = 3)
    res <- ga_aw_realtime(propertyId, brrr)
    
    return(res)
    
  }
  
  # here as not needed for real-time but needed for brrr
  dates <- gaw_dates(date_range)
  
  # brrr <- BatchRunReportsRequest(
  #   entity = Entity(propertyId),
  #   requests = list(
  #     RunReportRequest(
  #       entity = Entity(propertyId),
  #       metrics = mets,
  #       dimensions = dims,
  #       dateRanges = dates,
  #       limit = limit,
  #       dimensionFilter = dimensionFilter,
  #       metricFilter = metricFilter,
  #       metricAggregations = metricAggregations,
  #       orderBys = orderBys,
  #       keepEmptyRows = TRUE,
  #       returnPropertyQuota = TRUE
  #     )
  #   )
  # )
  
  brrr <- RunReportRequest(
        entity = Entity(propertyId),
        metrics = mets,
        dimensions = dims,
        dateRanges = dates,
        limit = limit,
        dimensionFilter = dimensionFilter,
        metricFilter = metricFilter,
        metricAggregations = metricAggregations,
        orderBys = orderBys,
        keepEmptyRows = TRUE,
        returnPropertyQuota = TRUE
      )
  
  ga_aw_report(brrr)
}

#' Realtime API
#' @noRd
ga_aw_realtime <- function(property, requestObj){
  
  url <- sprintf("https://analyticsdata.googleapis.com/%s/properties/%s:runRealtimeReport",
                 version_aw(), property)
  # analyticsdata.runRealtimeReport
  f <- gar_api_generator(url, "POST", 
                         data_parse_function = parse_realtime)
  
  stopifnot(inherits(requestObj, "gar_RunRealtimeReport"))
  o <- f(the_body = requestObj)
  
  o
}

#' Normal Reporting API
#' @noRd
ga_aw_report <- function(requestObj){
  
  url <- sprintf("https://analyticsdata.googleapis.com/%s:runReport",
                 version_aw())
  
  # analyticsdata.batchRunReports
  f <- gar_api_generator(url, "POST", 
                         data_parse_function = parse_runreport)
  
  stopifnot(inherits(requestObj, "gar_RunReportRequest"))
  o <- f(the_body = requestObj)
  
  o
}

#' Batched Reporting API
#' @noRd
ga_batch_report <- function(requestObj){
  
  url <- sprintf("https://analyticsdata.googleapis.com/%s:batchRunReports",
                 version_aw())
  
  # analyticsdata.batchRunReports
  f <- gar_api_generator(url, "POST", 
                         data_parse_function = parse_batchrunreports)
  
  stopifnot(inherits(requestObj, "gar_BatchRunReportsRequest"))
  o <- f(the_body = requestObj)
  
  o
}

parse_realtime <- function(x){
  if(no_rows(x)) return(data.frame())
  
  dim_names <- x$dimensionHeaders$name
  met_names <- x$metricHeaders$name
  
  parse_rows(x, dim_names, met_names)
  
}

parse_runreport <- function(o){
  
  if(no_rows(o)) return(data.frame())
  
  dim_names <- o$dimensionHeaders$name
  met_names <- o$metricHeaders$name
  
  parse_rows(o, dim_names, met_names)
}

parse_batchrunreports <- function(x){

  o <- x$reports
  
  if(no_rows(o)) return(data.frame())
  
  dim_names <- o$dimensionHeaders[[1]]$name
  met_names <- o$metricHeaders[[1]]$name
  
  parse_batch_rows(o, dim_names, met_names)
}

no_rows <- function(o){
  if(is.null(o$rows)){
    myMessage("No data found", level = 3)
    return(TRUE)
  }
  
  FALSE
}

row_types <- function(res, met_names){
  #type changes
  if("date" %in% names(res)){
    res$date <- as.Date(res$date, format = "%Y%m%d")
  }
  res %>% mutate(across(met_names, as.numeric))
}

#' @noRd
#' @importFrom dplyr bind_cols bind_rows across mutate
parse_rows <- function(o, dim_names, met_names){

  quota_messages(o)
  
  dds <- lapply(o$rows$dimensionValues, function(x) setNames(x$value, dim_names))
  mms <- lapply(o$rows$metricValues, function(x) setNames(x$value, met_names))
    
  res <- bind_cols(bind_rows(dds), bind_rows(mms))
  
  res <- row_types(res, met_names = met_names)

  attr(res, "metadata") <- o$metadata
  
  res
  
}


#' @noRd
#' @importFrom dplyr bind_cols across mutate
parse_batch_rows <- function(o, dim_names, met_names){
  quota_messages(o)
  
  the_data <- lapply(o$rows, function(x){
    dds <- get_value_cols(x, type = "dimensionValues")
    mms <- get_value_cols(x, type = "metricValues")
    dds <- setNames(dds, dim_names)
    mms <- setNames(mms, met_names)
    
    if(nrow(dds) == 0){
      o <- mms
    } else {
      o <- bind_cols(dds, mms)
    }
    
    o
    
  })
  
  res <- bind_cols(the_data)
  
  #type changes
  res <- row_types(res, met_names = met_names)

  attr(res, "metadata") <- o$metadata
  
  res
}

#' @noRd
#' @importFrom tibble as_tibble
get_value_cols <- function(x, 
                           type = c("dimensionValues", "metricValues")){
  type <- match.arg(type)
  as_tibble(
    do.call(rbind, lapply(x[[type]], function(y) y[["value"]])),
    .name_repair = "minimal")
  
} 