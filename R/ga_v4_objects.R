#' Make a date object
#'
#' @keywords internal
date_ga4 <- function(dvector){
  if(is.null(dvector)) return(NULL)

  assertthat::assert_that(length(dvector) == 2)

  dvector <- as.character(dvector)

  structure(
    list(startDate = dvector[1],
         endDate = dvector[2]),
    class = "date_ga4"
  )
}


#' Make a dimension object
#'
#' @param vector A character vector of dimension names
#' @param histogramBuckets Optional named list of histogram buckets vectors
#'
#' @section histograms:
#'
#'  If supplying histogramBuckets, the name will be used to create the buckets
#'
#'  e.g. \code{dimensions = c("source", "medium")
#'             histogramBuckets = list(source = c(1,5,10,20,200),
#'                                    medium = c(0,7,14,21,999))
#'                                    }
#'
#'  Warning: only makes sense to apply to dimensions that are numeric e.g. ga:hour
#'
#' @keywords internal
#' @return a list suitable for parsing in req
dimension_ga4 <- function(vector, histogramBuckets=NULL){
  
  if(is.null(vector)) return(NULL)

  assertthat::assert_that(is.character(vector))
  expect_null_or_type(histogramBuckets, "list")

  dimensions <- vapply(vector, checkPrefix, character(1), prefix="ga", USE.NAMES = FALSE)

  structure(
    lapply(dimensions, function(x) list(name = x,
                                        histogramBuckets =
                                          unname(histogramBuckets[gsub("ga:","",x)])
                                        )),
    class = "dim_ga4")

}

#' Make a metric object
#'
#' @param vector A character vector of GA metric names
#' @param metricFormat Optional same length character vector of formats
#'   for the API to return
#'
#' @keywords internal
#' @return a list suitable for parsing in req
metric_ga4 <- function(vector, metricFormat=NULL){

  assertthat::assert_that(is.character(vector))

  ## dont do prefix check for unnamed metrics if any named
  if(!is.null(names(vector))){
    do_prefix_check <- vector[names(vector) == ""]
    ## metrics may have a named vector so USE.NAMES must be TRUE
    metrics <- vapply(do_prefix_check, checkPrefix, character(1), prefix="ga", USE.NAMES = TRUE)
    metrics <- c(vector[names(vector) != ""], metrics)
  } else {
    do_prefix_check <- vector
    ## metrics may have a named vector so USE.NAMES must be TRUE
    metrics <- vapply(do_prefix_check, checkPrefix, character(1), prefix="ga", USE.NAMES = TRUE)
  }
  
  metrics
  


  if(is.null(metricFormat)) metricFormat <- rep("METRIC_TYPE_UNSPECIFIED",
                                                length(metrics))

  assertthat::assert_that(any(metricFormat %in% c("METRIC_TYPE_UNSPECIFIED",
                                                  "INTEGER",
                                                  "FLOAT",
                                                  "CURRENCY",
                                                  "PERCENT",
                                                  "TIME")),
                          length(metricFormat) == length(metrics))

  metrics <- lapply(seq_along(metrics), function(x) {
    entry <- metrics[x]
    al <- if(any(nchar(names(entry)) > 0, !is.null(names(entry)))) names(entry) else NULL
    list(expression = unname(entry),
         alias = al,
         formattingType = metricFormat[x])
  })

  metrics <- rmNullObs(metrics)

  structure(
    metrics,
    class = "met_ga4")

}


#' Make an OrderType object
#'
#' @param field One field to sort by
#' @param sort_order ASCENDING or DESCENDING
#' @param orderType Type of ordering
#'
#' @return A order_type_ga4 object for use in GAv4 fetch
#'
#' @details For multiple order sorting, create separate OrderType objects to pass
#'
#' @export
order_type <- function(field,
                       sort_order = c("ASCENDING", "DESCENDING"),
                       orderType = c("VALUE",
                                     "DELTA",
                                     "SMART",
                                     "HISTOGRAM_BUCKET",
                                     "DIMENSION_AS_INTEGER")){
  
  sort_order <- match.arg(sort_order)
  orderType <- match.arg(orderType)
  
  assertthat::assert_that(length(field) == 1)

  field <- vapply(field, checkPrefix, character(1), prefix = "ga")

  structure(
    list(
      fieldName = field,
      orderType = orderType,
      sortOrder = sort_order
    ),
    class = "order_type_ga4"
  )
}

