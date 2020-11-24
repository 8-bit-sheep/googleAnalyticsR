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
#' @family BatchRunReportsRequest functions
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
#'   dimensionFilter = ga_aw_filter("city", "Copenhagen", "EXACT"),
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
#'   dimensions = "city",
#'   dimensionFilter = ga_aw_filter("city", "Copenhagen", "EXACT"),
#'   limit = 100,
#'   realtime = TRUE)
#' 
#' }
ga_data <- function(propertyId,
                    metrics,
                    date_range,
                    dimensions = NULL,
                    dimensionFilter = NULL,
                    dimensionDelimiter = "/",
                    metricFilter = NULL,
                    # metricAggregations = NULL,
                    orderBys = NULL,
                    limit = 100,
                    realtime=FALSE) {
  
  # build a filterExpression if a Filter present
  if(is.Filter(dimensionFilter)){
    dimensionFilter <- ga_aw_filter_expr(dimensionFilter)
  }
  
  # build a filterExpression if a Filter present
  if(is.Filter(metricFilter)){
    metricFilter <- ga_aw_filter_expr(metricFilter)
  }  
  
  # if(!is.null(metricAggregations)){
  #   assert_that(all(metricAggregations %in% c("TOTAL",
  #                                              "MINIMUM",
  #                                              "MAXIMUM",
  #                                              "COUNT")))
  # }
  #TODO
  metricAggregations <- NULL
  
  dates <- gaw_dates(date_range)
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
  
  brrr <- BatchRunReportsRequest(
    entity = Entity(propertyId),
    requests = list(
      RunReportRequest(
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
    )
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

parse_batchrunreports <- function(x){
  
  o <- x$reports
  
  if(no_rows(o)) return(data.frame())
  
  dim_names <- o$dimensionHeaders[[1]]$name
  met_names <- o$metricHeaders[[1]]$name
  
  parse_rows(o, dim_names, met_names)
}

no_rows <- function(o){
  if(is.null(o$rows)){
    myMessage("No data found", level = 3)
    return(TRUE)
  }
  
  FALSE
}

#' @noRd
#' @importFrom dplyr bind_cols across mutate
parse_rows <- function(o, dim_names, met_names){
  
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
  
  res <- Reduce(rbind, the_data)
  
  #type changes
  if("date" %in% names(res)){
    res$date <- as.Date(res$date, format = "%Y%m%d")
  }
  res <- res %>% mutate(across(met_names, as.numeric))
  
  quota_messages(o)
  
  attr(res, "metadata") <- if(ncol(o$metadata) > 0) o$metadata else NULL
  
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