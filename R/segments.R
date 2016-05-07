#' Make a segment object for use
#'
#' A Segment is a subset of the Analytics data. 
#' For example, of the entire set of users, one Segment 
#' might be users from a particular country or city.
#' 
#' \code{segment_ga4} is the top heirarchy of segment creation, for which you will also need:
#' \itemize{
#'  \item \link{segment_define} : AND combination of segmentFilters
#'  \item \link{segment_vector_simple} or \link{segment_vector_sequence}
#'  \item \link{segment_element} that are combined in OR lists for \code{segment_vectors_*}
#' }
#' 
#' @param name The name of the segment for the reports.
#' @param segment_id The segment ID of a built in or custom segment e.g. gaid::-3
#' @param user_segment A list of \code{segment_define}'s that apply to users
#' @param session_segment A list of \code{segment_define}'s that apply to sessions
#'
#' @return a segmentFilter object. You can pass a list of these to the request.
#' @family v4 segment functions
#' @export
segment_ga4 <- function(name,
                        segment_id=NULL,
                        user_segment=NULL,
                        session_segment=NULL){
  
  testthat::expect_type(name, "character")
  expect_null_or_type(segment_id, "character")
  # expect_list_of_this(user_segment, "segmentDef_ga4", null_ok=TRUE)
  # expect_list_of_this(session_segment, "segmentDef_ga4", null_ok=TRUE)

  if(!is.null(segment_id)){
    out <- segmentObj_ga4(
      segmentId = segment_id
    )
    return(out)
  }
  
  segmentObj_ga4(
    dynamicSegment = dynamicSegment(name = name,
                                    userSegment = user_segment,
                                    sessionSegment = session_segment)
  )
  
}

#' Make a segment definition
#' 
#' Defines the segment to be a set of SegmentFilters 
#'   which are combined together with a logical AND operation.
#' 
#' @param segmentFilters A list of \link{segment_vector_simple} and \link{segment_vector_sequence}
#' @param not_vector Boolean applied to each segmentFilter step. 
#'   If NULL, assumed FALSE
#' 
#' @return segmentDefinition object for \link{segment_ga4}
#' @family v4 segment functions
#' @export
segment_define <- function(segment_filters,
                           not_vector=NULL){
  
  segment_filters <- unitToList(segment_filters)
  
  # expect_list_of_type(segment_filters,  c("list"))
  expect_null_or_type(not_vector, "list")
  
  if(is.null(not_vector)){
    not_vector <- as.list(rep(FALSE, length(segment_filters)))  
  }
  
  segment_filter_list <- mapply(function(sf, not){
      type <- class(sf)
      seqF <- NULL
      simF <- NULL
      if(type == "sequenceSegment_ga4"){
        seqF <- sf
      } else if(type == "simpleSegment_ga4"){
        simF <- sf
      } else{
        stop("Unrecognised type")
      }
      
      segmentFilter(not = not, 
                    simpleSegment = simF, 
                    sequenceSegment = seqF)
       
    }, segment_filters, not_vector, SIMPLIFY = FALSE)
  
  segmentDefinition(segment_filter_list)
  
}



#' Make a simple segment vector
#' 
#' @param segment_elements A list of OR lists of \link{segment_element}
#' 
#' @return A segment vector you can put in a list for use in \link{segment_ga4}
#' @family v4 segment functions
#' @export
segment_vector_simple <- function(segment_elements){
  
  testthat::expect_type(segment_elements, "list")
  
  orFiltersList <- makeOrFilters(segment_elements)
   
  expect_list_of_this(orFiltersList, "orFiltersForSegment_ga4")  
  
  simpleSegment(orFiltersList)

}

#' Make orFiltersForSegment
#' @keywords internal
makeOrFilters <- function(segment_element_list){
  
  lapply(segment_element_list, function(sfc){
      orFiltersForSegment(sfc)
  })
  
}

#' Make sequenceSegment
#' 
#' @param segment_elements a list of OR lists of segment elements
#' @param stepMatchList a list same length as segment_elements of
#'   c("PRECEDES", "IMMEDIATELY_PRECEDES") for each step. 
#'   If omitted, assumed all to be "PRECEDES"
#' @param firstStepMatch FALSE default
#' 
#' @family v4 segment functions   
#' @export
segment_vector_sequence <- function(segment_elements,
                                    stepMatchList=NULL,
                                    firstStepMatch=FALSE){
  
  if(!is.null(stepMatchList)){
    testthat::expect_equal(length(segment_elements), length(stepMatchList))
  } else {
    stepMatchList <- as.list(rep("PRECEDES", length(segment_elements)))
  }
  
  steps <- mapply(function(sfc, sml){
    
    orFilters <- makeOrFilters(sfc)
    segmentSequenceStep(orFilters, matchType = sml)
    
  }, segment_elements, stepMatchList, SIMPLIFY = FALSE)
  
  sequenceSegment(steps, firstStepMatch = firstStepMatch)
  
}

#' Make a segment element
#' 
#' Define the segment steps here, then combine into a vector and pass to \link{segment_vector}
#'   which in turn is combined into a list for \link{segment_ga4} 
#'   to finally be passed as a list to \link{google_analytics_4}
#'
#' @param name Name of the GA metric or dimension to segment on
#' @param operator How name shall operate on expression or comparisonValue
#' @param type A metric or dimension based segment element
#' @param not Should the element be the negation of what is defined
#' @param expressions [dim] What the name shall compare to
#' @param caseSensitive [dim] Whether to be case sensitive
#' @param minComparisonValue [dim] Minimum comparison values for BETWEEN
#' @param maxComparisonValue Max comparison value for BETWEEN operator
#' @param scope [met] Scope of the metric value
#' @param comparisonValue [met] What the name shall compare to
#' @param matchType If used in sequence segment, what behaviour
#'
#' @return An SegmentFilterClause object for use in a vector passed to \link{segment_vector}
#' @family v4 segment functions
#' @export
segment_element <- function(name,
                            operator = c("REGEXP",
                                         "BEGINS_WITH",
                                         "ENDS_WITH",
                                         "PARTIAL",
                                         "EXACT",
                                         "IN_LIST",
                                         "NUMERIC_LESS_THAN",
                                         "NUMERIC_GREATER_THAN",
                                         "NUMERIC_BETWEEN",
                                         "LESS_THAN",
                                         "GREATER_THAN",
                                         "EQUAL",
                                         "BETWEEN"),
                            type = c("metric", "dimension"),
                            not = FALSE,
                            expressions=NULL,
                            caseSensitive=NULL,
                            minComparisonValue=NULL,
                            maxComparisonValue=NULL,
                            scope=c("SESSION","USER","HIT","PRODUCT"),
                            comparisonValue=NULL,
                            matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){

  testthat::expect_type(name, "character")
  operator <- match.arg(operator)
  type <- match.arg(type)
  testthat::expect_type(not, "logical")
  expect_null_or_type(expressions, "character")
  expect_null_or_type(caseSensitive, "logical")  
  expect_null_or_type(minComparisonValue, "double")  
  expect_null_or_type(maxComparisonValue, "double")  
  scope <- match.arg(scope)
  expect_null_or_type(comparisonValue, "double")  
  matchType <- match.arg(matchType)
  
  name <- sapply(name, checkPrefix, prefix = "ga")
  
  if(type == "metric"){
    
    if(!operator %in% c("LESS_THAN","GREATER_THAN","EQUAL","BETWEEN")){
      stop('You have the wrong operator, 
           must be one of ("LESS_THAN","GREATER_THAN","EQUAL","BETWEEN")')
    }
    
    smf <- segmentMetricFilter(name = name,
                              scope = scope,
                              operator = operator,
                              comparisonValue = as.character(comparisonValue),
                              maxComparisonValue = as.character(maxComparisonValue))
    
    testthat::expect_s3_class(smf, "segmentMetFilter_ga4")
    
    sfc <- segmentFilterClause(not = not, metricFilter = smf)
    
  } else {
    
    stopifnot(!is.null(expressions))
    
    if(!operator %in% c(
      "REGEXP",
      "BEGINS_WITH",
      "ENDS_WITH",
      "PARTIAL",
      "EXACT",
      "IN_LIST",
      "NUMERIC_LESS_THAN",
      "NUMERIC_GREATER_THAN",
      "NUMERIC_BETWEEN"
    )){
      stop('You have the wrong operator, must be one of (
           "REGEXP",
           "BEGINS_WITH",
           "ENDS_WITH",
           "PARTIAL",
           "EXACT",
           "IN_LIST",
           "NUMERIC_LESS_THAN",
           "NUMERIC_GREATER_THAN",
           "NUMERIC_BETWEEN"
      )')
      
    }
    
    sdf <- segmentDimensionFilter(name = name,
                                 expressions = expressions,
                                 operator = operator,
                                 caseSensitive = caseSensitive,
                                 minComparisonValue = as.character(minComparisonValue),
                                 maxComparisonValue = as.character(maxComparisonValue))
    
    testthat::expect_s3_class(sdf, "segmentDimFilter_ga4")
    sfc <- segmentFilterClause(not = not, dimensionFilter = sdf)
    
  }
  
  sfc
  
}

#' Segment objects
#'
#' @param dynamicSegment A v4 segment object
#' @param segmentId A v3 segment string
#'
#' @return a list of class \code{segment_ga4}
#'
#' @family v4 segment functions
#' @keywords internal
segmentObj_ga4 <- function(dynamicSegment=NULL, segmentId=NULL){
  
  if(!is.null(dynamicSegment) && !is.null(segmentId))
    stop("Only one of either dynamicSegment or segmentExpression allowed")
  
  structure(
      list(
        list(
          dynamicSegment = dynamicSegment,
          segmentId = segmentId
        )

    ),
    class = "segment_ga4")
  
}

#' Dynamic Segment v4 object
#' 
#' @param name Name of dynamic segment
#' @param userSegment userSegment to include in the segment
#' @param sessionSegment sessionSegment to include in the segment
#'
#' @family v4 segment functions
#' @keywords internal
dynamicSegment <- function(name, userSegment, sessionSegment){
  
  testthat::expect_type(name, "character")
  
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
#' @param segmentFilterList A list of segmentFilters
#' 
#' SegmentDefinition defines the segment to be a set of 
#'   SegmentFilters which are combined together with a logical AND operation.
#' 
#' @family v4 segment functions
#' @keywords internal
segmentDefinition <- function(segmentFilterList){
  
  # expect_list_of_this(segmentFilterList,  "segmentFilter_ga4")
  
  structure(
    list(
      segmentFilters = segmentFilterList
    ),
    class = "segmentDef_ga4"
  )
}

#' Segment Filter
#'
#' @param not Negation of this filter
#' @param simpleSegment An simple segment
#' @param sequenceSegment An sequence segment
#'
#' @details
#'
#' If true, match the complement of simple or sequence segment above.
#' For example, to match all visits not from "New York", set to TRUE.
#'
#' @family v4 segment functions
#' @keywords internal
segmentFilter <- function(not=FALSE, simpleSegment=NULL, sequenceSegment=NULL){
  
  testthat::expect_type(not, "logical")
  
  if(!is.null(simpleSegment) && !is.null(sequenceSegment)){
    stop("Only one of either simpleSegment or sequenceSegment allowed")
  }

  if(all(is.null(simpleSegment), is.null(sequenceSegment))){
    stop("No segments passed, both NULL")
  }
  
  structure(
    list(
      not = not,
      simpleSegment = simpleSegment,
      sequenceSegment = sequenceSegment
    ),
    class = "segmentFilter_ga4"
  )
  
}

#' Simple Segment
#'
#' @family v4 segment functions
#' @keywords internal
simpleSegment <- function(orFiltersForSegmentList){
  
  expect_list_of_this(orFiltersForSegmentList, "orFiltersForSegment_ga4")
  
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
#' @keywords internal
orFiltersForSegment <- function(segmentFilterClauseList){
  
  # expect_list_of_this(testthat::expect_s3_class, segmentFilterClauseList, "segmentFilterClause_ga4")
  
  structure(
    list(
      segmentFilterClauses = segmentFilterClauseList
    ),
    class = "orFiltersForSegment_ga4"
  )
  
}

#' segmentFilterClause
#'
#' Make this internal
#'
#' @family v4 segment functions
#' @keywords internal
segmentFilterClause <- function(not=FALSE,
                                dimensionFilter=NULL,
                                metricFilter=NULL){
  
  if(!is.null(dimensionFilter) && !is.null(metricFilter))
    stop("Only one of either dimensionFilter or metricFilter allowed")
  
  structure(
    list(not = not,
         dimensionFilter = dimensionFilter,
         metricFilter = metricFilter
    ),
    class = "segmentFilterClause_ga4"
  )
  
}

#' segmentDimensionFilter
#'
#' @family v4 segment functions
#' @keywords internal
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
#' @keywords internal
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
#' @param segmentSequenceStepList A list of \link{segmentSequenceStep}
#' @param firstStepMatch Should firstStep match the first hit?
#' 
#' @return A sequenceSegment object
#'
#' @family v4 segment functions
#' @keywords internal
sequenceSegment <- function(segmentSequenceStepList, firstStepMatch=FALSE){
  
  # expect_list_of_this(testthat::expect_s3_class, segmentSequenceStepList,  "segmentSequenceStep_ga4")
  testthat::expect_type(firstStepMatch, "logical")
  
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
#' @param orFiltersForSegmentList A list of \link{orFiltersForSegment}
#' @param matchType Should it precede or immediatly precede the next step
#' 
#' @return segmentSequenceStep object
#' 
#' @family v4 segment functions
#' @keywords internal
segmentSequenceStep <- function(orFiltersForSegmentList,
                                matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){

  # expect_list_of_this(testthat::expect_s3_class, orFiltersForSegmentList,  "orFiltersForSegment_ga4")
  matchType <- match.arg(matchType)
  
  structure(
    list(
      orFiltersForSegment = orFiltersForSegmentList,
      matchType = matchType
    ),
    class = "segmentSequenceStep_ga4"
  )
  
}

