#' Make a date object
#'
#'
date_ga4 <- function(vector){
  if(is.null(vector)) return(NULL)

  stopifnot(length(vector) == 2)

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
                       orderType = c("VALUE","DELTA","SMART","HISTOGRAM_BUCKET","DIMENSION_AS_INTEGER")){
  orderType <- match.arg(orderType)

  stopifnot(length(field) == 1, inherits(field, "character"))

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

#' Make a pivot object
#'
#' @param pivot_dim A character vector of dimensions
#' @param metrics Metrics to aggregate and return.
#' @param dim_filter_clause Only data included in filter included.
#' @param startGroup which groups of k columns are included in response.
#' @param maxGroupCount Maximum number of groups to return.
#'
#' @details If maxGroupCount is set to -1 returns all groups.
#'
#' @return pivot object of class \code{pivot_ga4} for use in \code{\link{filter_clause_ga4}}
#'
#' @export
pivot_ga4 <- function(pivot_dim, metrics, dim_filter_clause=NULL,
                      startGroup = 1, maxGroupCount = 5){

  stopifnot(inherits(pivot_dim, "character"),
            inherits(metrics, "character"),
            any(is.null(dim_filter_clause), inherits(dim_filter_clause, "list")))

  dim_list <- dimension_ga4(pivot_dim)
  met_list <- metric_ga4(metrics)

  if(!is.null(dim_filter_clause)) stopifnot(inherits(dim_filter_clause, "dim_fil_ga4"))

  structure(
    list(
      dimensions = dim_list,
      dimensionFilterClauses = dim_filter_clause,
      metrics = met_list,
      startGroup = startGroup,
      maxGroupCount = maxGroupCount
    ),
    class = "pivot_ga4"

  )


}

#' Segment objects
#'
#' @param dynamicSegment A v4 segment object
#' @param segmentExpression A v3 segment string
#'
#' @return a list of class \code{segment_ga4}
#'
#' @family v4 segment functions
segmentObj_ga4 <- function(dynamicSegment=NULL, segmentExpression=NULL){

  if(!is.null(dynamicSegment) && !is.null(segmentExpression))
    stop("Only one of either dynamicSegment or segmentExpression allowed")

  structure(
    list(
    dynamicSegment = dynamicSegment,
    segmentExpression = segmentExpression
  ),
  class = "segment_ga4")

}

#' Dynamic Segment v4 object
#'
#' @family v4 segment functions
dynamicSegment <- function(name, userSegment, sessionSegment){

  structure(
    list(
      name = name,
      userSegment = userSegment,
      sessionSegment = sessionSegment
    ),
    class = "dynamicSegment_ga4"
  )

}

#' Segment Definition
#'
#' @family v4 segment functions
segmentDefinition <- function(segmentFilterList){
  stopifnot(inherits(segmentFilterList, "list"))

  structure(
    segmentFilters = segmentFilterList,
    class = "segmentDef_ga4"
  )
}

#' Segment Filter
#'
#' @param matchComplement
#' @param simpleSegment
#' @param sequenceSegment
#'
#' @details
#'
#' If true, match the complement of simple or sequence segment above.
#' For example, to match all visits not from "New York", set to TRUE.
#'
#' @family v4 segment functions
segmentFilter <- function(matchComplement=FALSE, simpleSegment=NULL, sequenceSegment=NULL){
  if(!is.null(simpleSegment) && !is.null(sequenceSegment))
    stop("Only one of either simpleSegment or sequenceSegment allowed")

  structure(
    list(
      matchComplement = matchComplement,
      simpleSegment = simpleSegment,
      sequenceSegment = sequenceSegment
    ),
    structure = "segmentFilter_ga4"
  )

}

#' Simple Segment
#'
#' @family v4 segment functions
simpleSegment <- function(orFiltersForSegmentList){
  stopifnot(inherits(orFiltersForSegmentList, "list"))

  structure(
    list(
      orFiltersForSegment = orFiltersForSegmentList
    ),
    class = "simpleSegment_ga4"
  )

}

#' orFiltersForSegment
#'
#' @family v4 segment functions
orFiltersForSegment <- function(segmentFilterClause){

  structure(
    list(
      segmentFilterClause = segmentFilterClause
    ),
    class = "orFiltersForSegment_ga4"
  )

}

#' segmentFilterClause
#'
#' Make this internal
#'
#' @family v4 segment functions
segmentFilterClause <- function(matchComplement=FALSE,
                                dimensionFilter=NULL,
                                metricFilter=NULL){

  if(!is.null(dimensionFilter) && !is.null(metricFilter))
    stop("Only one of either dimensionFilter or metricFilter allowed")

  structure(
   list(matchComplement = matchComplement,
        dimensionFilter = dimensionFilter,
        metricFilter = metricFilter
   ),
   class = "segmentFilterClause_ga4"
  )

}

#' segmentDimensionFilter
#'
#' @family v4 segment functions
segmentDimensionFilter <- function(name,
                                   expressions,
                                   operator = c(
                                     "REGEXP",
                                     "BEGINS_WITH",
                                     "ENDS_WITH",
                                     "PARTIAL",
                                     "EXACT",
                                     "IN_LIST",
                                     "NUMERIC_LESS_THAN",
                                     "NUMERIC_GREATER_THAN",
                                     "NUMERIC_BETWEEN"
                                   ),
                                   caseSensitive=FALSE,
                                   minComparisonValue=NULL,
                                   maxComparisonValue=NULL){
  operator <- match.arg(operator)

  if(operator == "NUMERIC_BETWEEN" && any(is.null(minComparisonValue), is.null(maxComparisonValue))) stop("Need both min/max Comparison Value when operator = NUMERIC_BETWEEN")

  structure(
    list(
      dimensionName = name,
      operator = operator,
      caseSensitive = caseSensitive,
      expressions = expressions,
      minComparisonValue = minComparisonValue,
      maxComparisonValue = maxComparisonValue
    ),
    class = "segmentDimFilter_ga4"
  )

}

#' segmentMetricFilter
#'
#' @family v4 segment functions
segmentMetricFilter <- function(name,
                                scope = c("PRODUCT", "HIT","SESSION","USER"),
                                operator = c("LESS_THAN","GREATER_THAN","EQUAL","BETWEEN"),
                                comparisonValue=NULL,
                                maxComparisonValue=NULL){

  scope <- match.arg(scope)
  operator <- match.arg(operator)

  if(operator == "BETWEEN" && any(is.null(comparisonValue), is.null(maxComparisonValue))) stop("Need both comparisonValue and maxComparisonValue when operator = BETWEEN")

  structure(
    list(
      scope = scope,
      metricName = name,
      operator = operator,
      comparisonValue = comparisonValue,
      maxComparisonValue = maxComparisonValue
    ),
    class = "segmentMetFilter_ga4"
  )
}

#' sequenceSegment
#'
#' @family v4 segment functions
sequenceSegment <- function(segmentSequenceStepList, firstStepMatch=FALSE){
  stopifnot(inherits(segmentSequenceStepList, "list"))

  structure(
    list(
      segmentSequenceSteps = segmentSequenceStepList,
      firstStepShouldMatchFirstHit = firstStepMatch
    ),
    class = "sequenceSegment_ga4"
  )
}

#' segmentSequenceStep
#'
#' @family v4 segment functions
#'
segmentSequenceStep <- function(orFiltersForSegmentList,
                                matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){
  matchType <- match.arg(matchType)

  stopifnot(inherits(orFiltersForSegmentList,"list"))

  structure(
    list(
     orFiltersForSegment = orFiltersForSegmentList,
     matchType = matchType
    ),
    class = "segmentSequenceStep_ga3"
  )

}
