version_aw <- function(){
  "v1alpha"
}


#' Google Analytics for GA4 (App+Web)
#'
#' Fetches Google Analytics data for Google Analytics 4 (Previously App+Web)
#'
#' @seealso \href{https://developers.google.com/analytics/trusted-testing/analytics-data/}{Google Documentation}
#'
#' @details
#'
#' @inheritParams Entity
#' @inheritParams RunReportRequest
#' @param date_range A vector of length two with start and end dates in YYYY-MM-DD format
#' @param dimensionDelimiter If combining dimensions in one column, the delimiter for the value field
#' @param realtime If TRUE then will call the real-time reports, that have a more limited set of dimensions/metrics - see \url{https://developers.google.com/analytics/trusted-testing/analytics-data/realtime-basics}
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family BatchRunReportsRequest functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' # send up to 4 date ranges
#' multi_date <- google_analytics_aw(
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
#' met_expression <- google_analytics_aw(
#'   206670707,
#'   metrics = c("activeUsers","sessions",sessionsPerUser = "sessions/activeUsers"),
#'   dimensions = c("date","city","dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27"),
#'   dimensionFilter = ga_aw_filter("city", "Copenhagen", "EXACT"),
#'   limit = 100
#'   )
#'
#' # create your own aggregation dimensions
#' dim_expression <- google_analytics_aw(
#'   206670707,
#'   metrics = c("activeUsers","sessions"),
#'   dimensions = c("date","city","dayOfWeek", cdow = "city/dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27"),
#'   dimensionFilter = ga_aw_filter("city", "Copenhagen", "EXACT"),
#'   limit = 100
#'   )
#'   
#' # run a real-time report (no date dimension allowed)
#' realtime <- google_analytics_aw(
#'   206670707,
#'   metrics = "activeUsers",
#'   dimensions = "city",
#'   dimensionFilter = ga_aw_filter("city", "Copenhagen", "EXACT"),
#'   limit = 100,
#'   realtime = TRUE)
#' 
#' }
google_analytics_aw <- function(propertyId,
                                metrics,
                                date_range,
                                dimensions = NULL,
                                dimensionFilter = NULL,
                                dimensionDelimiter = "/",
                                metricFilter = NULL,
                                # metricAggregations = NULL,
                                orderBys = NULL,
                                limit = 100,
                                realtime=FALSE
                                ) {
  
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


#' @import assertthat
#' @noRd
gaw_dates <- function(date_range){
  # create up to 4 date ranges
  assert_that((length(date_range) %% 2) == 0,
              length(date_range) <= 8)
  
  dateRanges <- split(date_range, ceiling(seq_along(date_range)/2))
  
  unname(lapply(dateRanges, date_ga4))
}

gaw_metric <- function(metrics){
  unnamed_metrics <- metrics[names(metrics) == ""]
  named_metrics <- metrics[names(metrics) != ""]
  
  if(is.null(names(metrics))){
    unnamed_metrics <- metrics
  }
  
  all_metrics <- lapply(unnamed_metrics, function(x) Metric(name = x))
  
  if(length(named_metrics) > 0){
    metric_exp <- lapply(names(named_metrics), function(x){
      Metric(name = x, expression = unname(named_metrics[x]))
    })
    
    all_metrics <- c(all_metrics, metric_exp)
  }
  
  unname(all_metrics)

}

gaw_dimension <- function(dimensions, delimiter = "/"){

  if(is.null(dimensions)) return(NULL)
  unnamed_dims <- dimensions[names(dimensions) == ""]
  named_dims   <- dimensions[names(dimensions) != ""]
  
  if(is.null(names(dimensions))){
    unnamed_dims <- dimensions
  }
  
  all_dims <- lapply(unnamed_dims, function(x) Dimension(name = x))
  
  if(length(named_dims) > 0){
    dim_exp <- lapply(names(named_dims), function(x){
      Dimension(name = x, 
        dimensionExpression = list(
          concatenate = list(
            dimensionNames = strsplit(unname(named_dims[x]),delimiter)[[1]],
            delimiter = delimiter
          )
        )
      )
    })
    
    all_dims <- c(all_dims, dim_exp)
  }
  
  unname(all_dims)
}

quota_messages <- function(o){
  pq <- o$propertyQuota
  care_factor <- 0.5
  verbose <- getOption("googleAuthR.verbose") < 3

  # quota messages
  if(pq$tokensPerDay$consumed > (care_factor*pq$tokensPerDay$remaining) ||
     verbose){
    myMessage("tokensPerDay: Query Cost [", pq$tokensPerDay$consumed, 
              "] / Remaining [", pq$tokensPerDay$remaining,"]",
              level = 3)
  }
  
  if(pq$tokensPerHour$consumed > (care_factor*pq$tokensPerHour$remaining) ||
     verbose){
    myMessage("tokensPerHour: Query Cost [", pq$tokensPerHour$consumed, 
              "] / Remaining [", pq$tokensPerHour$remaining,"]",
              level = 3)
  }
  
  if(pq$concurrentRequests < 10 || verbose){
    myMessage("concurrentRequests: ", pq$concurrentRequests, 
              " / 10",
              level = 3)
  }
  
  if(pq$serverErrorsPerProjectPerHour < 10 || verbose){
    myMessage("serverErrorsPerProjectPerHour: ", pq$serverErrorsPerProjectPerHour, 
              " / 10",
              level = 3)
  }
}

#' Create a filter for use within App+Web filter expressions
#' 
#' Used within \link{ga_aw_filter_expr}
#' 
#' @param field The dimension or metric field name
#' @param value The value - filter type is based on what class of object
#' @param operation How the filter operates on field with value
#' @param caseSensitive For character based value filters
#' 
#' @details 
#' 
#' Ensure your value is of the right R class for the type of filter you desire.
#' 
#' \itemize{
#'   \item{character: string filter}
#'   \item{character vector: in list filter}
#'   \item{numeric: Numeric filter}
#'   \item{numeric 2-length vector: between filter}
#'   \item{logical: TRUE will filter for NULLs}
#'  }
#' 
#' For numerics also make sure to specify integer or float for metrics e.g.
#' 1L vs 1.00
#' 
#' Ensure operation is valid for the type of field you are passing:
#' 
#' string filters: "EXACT","BEGINS_WITH","ENDS_WITH","CONTAINS","FULL_REGEXP",
#'   "PARTIAL_REGEXP"
#'   
#' numeric filters: "EQUAL", "LESS_THAN", "LESS_THAN_OR_EQUAL", "GREATER_THAN",
#'   "GREATER_THAN_OR_EQUAL")
#'   
#'   
#' 
#' @export
#' @importFrom assertthat is.string is.count
ga_aw_filter <- function(field,
                         value, 
                         operation = c("EXACT",
                                       "BEGINS_WITH",
                                       "ENDS_WITH",
                                       "CONTAINS",
                                       "FULL_REGEXP",
                                       "PARTIAL_REGEXP",
                                       "EQUAL",
                                       "LESS_THAN",
                                       "LESS_THAN_OR_EQUAL",
                                       "GREATER_THAN",
                                       "GREATER_THAN_OR_EQUAL"),
                         caseSensitive = TRUE){
  
  if(is.string(value)){
    assert_that(operation %in% c("EXACT",
                                 "BEGINS_WITH",
                                 "ENDS_WITH",
                                 "CONTAINS",
                                 "FULL_REGEXP",
                                 "PARTIAL_REGEXP"))
    o <- Filter_aw(field, 
                   stringFilter = StringFilter(
                     value,
                     matchType = operation,
                     caseSensitive = caseSensitive
                   ))
  } else if(is.character(value)){
    o <- Filter_aw(field,
                   inListFilter = InListFilter(
                     value,
                     caseSensitive = caseSensitive
                   ))
  } else if(is.count(value)){
    assert_that(operation %in% c("EQUAL",
                                 "LESS_THAN",
                                 "LESS_THAN_OR_EQUAL",
                                 "GREATER_THAN",
                                 "GREATER_THAN_OR_EQUAL"))
    o <- Filter_aw(field,
                   numericFilter = NumericFilter(
                     value,
                     operation = operation
                   ))
  } else if(is.numeric(value) && length(value) == 2){
    o <- Filter_aw(field,
                   betweenFilter = BetweenFilter(
                     fromValue = value[[1]],
                     toValue = value[[2]]
                   ))
  } else if(is.flag(value)){
    o <- Filter_aw(field, nullFilter = value)
  } else {
    stop("Didn't know what to do with value of type: ", class(value), 
         call. = FALSE)
  }
  
  o
  
}

#' Create a filter expression for use with App+Web Reports
#' 
#' For use with \link{google_analytics_aw} - filter expressions are built up via filters or other nested filter expressions.
#' 
#' @param ... Vectors of \link{ga_aw_filter}, or vectors of FilterExpression created from previous calls to this function.
#' @param type The type of filter
#' 
#' @return A FilterExpression
#' 
#' @details The fields in a FilterExpression need to be either all dimensions or all metrics. Use them in the appropriate dimensionFilter or metricFilter arguments within \link{google_analytics_aw}
#' 
#' You can't pass a mix of filters and filter expressions, or multiple filter expressions with type="not" as it doesn't know how to combine the filters (and/or) - make a filter expression with type = "and/or" then pass that back into the function with type="not" to negate the filter expression.
#' 
#' @export
#' @examples 
#' 
#' simple <- ga_aw_filter_expr(ga_aw_filter("city", "Copenhagen", "EXACT"))
#' 
#' multiple <- ga_aw_filter_expr(
#'       ga_aw_filter("dayOfWeek", "4", "EXACT"),
#'      ga_aw_filter("dayOfWeek", "5", "EXACT"),
#'     type = "or")
#'    
#'# equivalent to above as uses InList filter
#' multiple_or2 <- ga_aw_filter_expr(
#'       ga_aw_filter("dayOfWeek", c("4","5")))
#' 
#' # make a filter expression then pass it again with type='not' to negate it 
#' negative <- ga_aw_filter_expr(multiple_or2, type = "not")
#' 
#' # or directly - 

#' clean_city <- ga_aw_filter_expr(
#'   ga_aw_filter("city", "(not set)", "EXACT",), type = "not")
#'
#' # use previously created FilterExpressions to build up more complex filters
#' complex <- ga_aw_filter_expr(multiple_or2, clean_city)
#' 
#'    
#' 
ga_aw_filter_expr <- function(..., 
                              type = c("default","not","and","or")){
  type <- match.arg(type)
  dots <- list(...)
  
  # a list of Filters
  areFilters <- all(vapply(dots, is.Filter, FUN.VALUE = logical(1)))
  areFilterExpressions <- all(vapply(dots, is.FilterExpression, 
                                     FUN.VALUE = logical(1)))
  
  if(areFilters){
    
    return(make_filter_expr(dots, type))
  }
  
  if(areFilterExpressions){
    return(make_filter_expr_list(dots, type))
  }

  stop("Inconsistent filter objects - must all be Filters or FilterExpressions", 
       call. = FALSE)
  
}

make_filter_expr_list <- function(dots, type){
  myMessage("Got FilterExpressions", level = 2)
  # turn into a list of filterExpressionLists
  assert_that_list(dots, is.FilterExpression)
  if(type == "default"){
    type <- "and"
  }
  
  if(type == "not"){
    if(length(dots) > 1){
      stop("Invalid type='not' for multiple FilterExpressions (we don't know how to combine type=and/or) - build FilterExpression first and then pass back to ga_aw_filter_expr() with type = 'not'", call. = FALSE)
    }
    return(construct_filter_expr(dots[[1]], type = "not"))
  }
  
  assert_that(type %in% c("and","or"))
  
  filter_list <- FilterExpressionList(dots)
  
  construct_filter_expr(filter_list, type = type)
}

make_filter_expr <- function(dots, type){

  assert_that_list(dots, is.Filter)
  
  if(length(dots) == 1 && type %in% c("and","or")){
    warning("Ignoring argument 'type': only one filter. type=", type)
  }
  
  if(length(dots) == 1 && type == "default"){
    # a simple filter
    return(construct_filter_expr(dots[[1]], type = "default"))
  }
  
  # length > 1 of Filters - make list of filterExpressions
  lots_filters <- lapply(dots, construct_filter_expr, type = "default")
  
  make_filter_expr_list(lots_filters, type = type)

}



#' Constructs filterExpressions
#' @param a_filter A Filter object or a FilterExpression or a list of FilterExpressions
#' @param type The type of filter
#' @noRd
construct_filter_expr <- function(a_filter,
                                 type = c("default","not","and","or")){
  
  type <- match.arg(type)

  switch(type,
    default = FilterExpression(filter = a_filter),
    not = FilterExpression(notExpression = a_filter),
    and = FilterExpression(andGroup = a_filter),
    or = FilterExpression(orGroup = a_filter))
  
}




