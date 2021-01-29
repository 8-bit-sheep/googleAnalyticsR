version_aw <- function(){
  "v1alpha"
}

#' Google Analytics Data for GA4 (App+Web)
#'
#' Fetches Google Analytics from the Data API for Google Analytics 4 (Previously App+Web)
#'
#' @seealso \href{https://developers.google.com/analytics/devguides/reporting/data/v1}{Documentation on Data API}
#'
#' @details 
#' 
#' This is the main function to call the Google Analytics 4 Data API.  
#'
#' @inheritParams Entity
#' @inheritParams RunReportRequest
#' @param date_range A vector of length two with start and end dates in YYYY-MM-DD format
#' @param dimensionDelimiter If combining dimensions in one column, the delimiter for the value field
#' @param realtime If TRUE then will call the real-time reports, that have a more limited set of dimensions/metrics - see \href{https://developers.google.com/analytics/devguides/reporting/data/v1/realtime-basics}{valid real-time dimensions}
#' @param raw_json You can send in the raw JSON string for a Data API request which will skip all checks
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family GA4 functions
#' 
#' @return A data.frame tibble, including attributes metadata, metricAggregations and rowCount.  Use \link{ga_data_aggregations} to extract the data.frames of metricAggregations
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
#'   metricAggregations = c("TOTAL", "MINIMUM", "MAXIMUM"),
#'   limit = 100,
#'   realtime = TRUE)
#' 
#' # extract meta data from the table
#' ga_data_aggregations(realtime)
#' 
#' # add ordering
#' a <- ga_data_order(-sessions)
#' b <- ga_data_order(-dayOfWeek, type = "NUMERIC")
#' 
#' ga_data(
#'   206670707,
#'   metrics = c("activeUsers","sessions"),
#'   dimensions = c("date","city","dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27"),
#'   orderBys = c(a, b)
#'   )
#' }
ga_data <- function(
  propertyId,
  metrics,
  date_range = NULL,
  dimensions = NULL,
  dimensionFilter = NULL,
  dimensionDelimiter = "/",
  metricFilter = NULL,
  orderBys = NULL,
  metricAggregations = NULL,
  limit = 100,
  realtime = FALSE,
  raw_json = NULL) {
  
  if(!is.null(raw_json)){
    if(is.list(raw_json)){
      raw_json_txt <- jsonlite::toJSON(raw_json)
    } else {
      raw_json_txt <- raw_json
    }
    myMessage("Making API request with raw JSON: ", raw_json_txt, level = 3)
    
    if(realtime) return(ga_aw_realtime(propertyId, raw_json))
    
    return(ga_aw_report(raw_json))
  }
  
  # in case someone passes in a filter instead of an expression
  dimensionFilter <- as_filterExpression(dimensionFilter)
  metricFilter    <- as_filterExpression(metricFilter)
 
  # # we always get these 3 - COUNT is not available unless pivot?
  if(!is.null(metricAggregations)){
    assert_that(all(metricAggregations %in% c("TOTAL","MAXIMUM","MINIMUM")))
  }
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
  
  url <- 
    sprintf("https://analyticsdata.googleapis.com/%s/properties/%s:runRealtimeReport",
                 version_aw(), property)
  # analyticsdata.runRealtimeReport
  f <- gar_api_generator(url, "POST", 
                         data_parse_function = parse_realtime)
  
  o <- f(the_body = requestObj)
  
  o
}

#' Normal Reporting API
#' @noRd
ga_aw_report <- function(requestObj){
  
  url <- sprintf("https://analyticsdata.googleapis.com/%s:runReport",
                 version_aw())
  
  # analyticsdata.runReport
  f <- gar_api_generator(url, "POST", 
                         data_parse_function = parse_runreport)
  
  o <- f(the_body = requestObj)
  
  o
}


parse_realtime <- function(x){
  if(no_rows(x)) return(data.frame())
  
  dim_names <- x$dimensionHeaders$name
  met_names <- x$metricHeaders$name
  
  parse_rows(x, dim_names, met_names)
  
}

#' @noRd
parse_runreport <- function(o){

  if(no_rows(o)) return(data.frame())
  
  dim_names <- o$dimensionHeaders$name
  met_names <- o$metricHeaders$name
  
  parse_rows(o, dim_names, met_names)
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
  
  if("firstTouchDate" %in% names(res)){
    res$firstTouchDate <- as.Date(res$firstTouchDate, format = "%Y%m%d")
  }
  res %>% mutate(across(met_names, as.numeric))
}

get_field_values <- function(x, name){
  o <- lapply(x, function(y) setNames(y$value, name))
  bind_rows(o)
}

my_bind_cols <- function(x, y){
  # bind_cols returns 0rows if first df has 0
  if(nrow(x) == 0){
    return(y)
  }
  
  if(nrow(y) == 0){
    return(x)
  }
  
  bind_cols(x, y)
}

#' @noRd
#' @importFrom dplyr bind_cols bind_rows across mutate
parse_aggregations <- function(agg, dim_names, met_names){
  
  if(is.null(agg)) return(NULL)
  
  dds <- get_field_values(agg$dimensionValues, name = dim_names)
  mms <- get_field_values(agg$metricValues, name = met_names)  
  
  res <- my_bind_cols(dds, mms)
  
  res <- row_types(res, met_names)
  
  res
}

#' @noRd
#' @importFrom dplyr bind_cols bind_rows across mutate
parse_rows <- function(o, dim_names, met_names){

  quota_messages(o)

  dds <- get_field_values(o$rows$dimensionValues, name = dim_names)
  mms <- get_field_values(o$rows$metricValues, name = met_names)
  
  res <- my_bind_cols(dds, mms)
  
  res <- row_types(res, met_names = met_names)
  
  dl <- attr(res, "metadata")[["dataLossFromOtherRow"]]
  if(!is.null(dl) && dl){
    myMessage("Warning: some buckets of dimension combinations are rolled into '(other)' row. This can happen for high cardinality reports.", level = 3)
  }
  
  if(!is.null(o$metadata) && length(o$metadata) > 1){
    attr(res, "metadata") <- o$metadata
  }
  attr(res, "metricAggregations") <- list(
    totals =   parse_aggregations(o$totals, dim_names, met_names),
    maximums = parse_aggregations(o$maximums, dim_names, met_names),
    minimums = parse_aggregations(o$minimums, dim_names, met_names)
  )
  attr(res, "rowCount") <- o$rowCount
  
  ## remove dateRange column if only one unique value
  if(!is.null(res[["dateRange"]]) &&
     all(unique(res$dateRange) == "date_range_0")){
    res$dateRange <- NULL
  }
  
  res
  
}

#' Extract metric aggregations from a \link{ga_data} result
#' 
#' Metric aggregations are available in each \link{ga_data} result.  This function lets you easily access the data.frames
#' 
#' @param df A data.frame result from \link{ga_data}
#' @param type totals, maximums, minimums, counts (if available) or all
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' #' # send up to 4 date ranges
#' multi_date <- ga_data(
#'   206670707,
#'   metrics = c("activeUsers","sessions"),
#'   dimensions = c("date","city","dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27", "2020-04-30", "2020-05-27"),
#'   dimensionFilter = ga_data_filter("city"=="Copenhagen"),
#'   limit = 100
#'   )
#'
#' # metric aggregations for each date range
#' ga_data_aggregations(multi_date, type = "all")
#' 
#' # specify type
#' ga_data_aggregations(multi_date, type = "maximums")
#' 
#' }
ga_data_aggregations <- function(df, 
                                 type = c("all","totals",
                                          "maximums","minimums",
                                          "count")){
  type <- match.arg(type)
  if(is.null(attr(df, "metricAggregations"))){
    stop("No aggregations found.  Is the data.frame from ga_data()?", 
         call. = FALSE)
  }
  
  ma <- attr(df, "metricAggregations")
  
  if(type == "all"){
    return(ma)
  }
  
  ma[[type]]
  
}
