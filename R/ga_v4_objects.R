#' Make a date object
#'
#'
date_ga4 <- function(vector){
  if(is.null(vector)) return(NULL)

  testthat::expect_length(vector, 2)

  vector <- as.character(vector)

  structure(
    list(startDate = vector[1],
         endDate = vector[2]),
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
#'
#' @return a list suitable for parsing in req
dimension_ga4 <- function(vector, histogramBuckets=NULL){
  if(is.null(vector)) return(NULL)

  stopifnot(inherits(vector, "character"),
            any(inherits(histogramBuckets, "list"), is.null(histogramBuckets)))

  dimensions <- sapply(vector, checkPrefix, prefix="ga", USE.NAMES = F)

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
#' @return a list suitable for parsing in req
metric_ga4 <- function(vector, metricFormat=NULL){

  stopifnot(inherits(vector, "character"))

  metrics <- sapply(vector, checkPrefix, prefix="ga", USE.NAMES = F)

  if(is.null(metricFormat)) metricFormat <- rep("METRIC_TYPE_UNSPECIFIED",
                                                length(metrics))

  stopifnot(any(metricFormat %in% c("METRIC_TYPE_UNSPECIFIED",
                                     "INTEGER",
                                     "FLOAT",
                                     "CURRENCY",
                                     "PERCENT",
                                     "TIME"))
            )
  stopifnot(length(metricFormat) == length(metrics))

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
#' @param descending If the sorting should be descending or not
#' @param orderType Type of ordering
#'
#' @return A order_type_ga4 object for use in GAv4 fetch
#'
#' @details For multiple order sorting, create seperate OrderType objects to pass
#'
#' @export
order_type <- function(field,
                       descending = FALSE,
                       orderType = c("VALUE",
                                     "DELTA",
                                     "SMART",
                                     "HISTOGRAM_BUCKET",
                                     "DIMENSION_AS_INTEGER")){
  
  orderType <- match.arg(orderType)
  
  testthat::expect_length(field, 1)
  testthat::expect_type(field, character)

  field <- sapply(field, checkPrefix, prefix = "ga")
  if(descending) field <- paste(field, "desc")

  structure(
    list(
      fieldName = field,
      orderType = orderType
    ),
    class = "order_type_ga4"
  )
}

