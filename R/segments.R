#' Get segments user has access to
#'
#' @return Segment list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_segment_list <- function(){
  
  url <- "https://www.googleapis.com/analytics/v3/management/segments"
  segs <- gar_api_generator(url,
                            "GET",
                            data_parse_function = function(x) x)
  
  segs()
  
}

#' Make a segment object for use
#'
#' A Segment is a subset of the Analytics data. 
#' For example, of the entire set of users, one Segment 
#' might be users from a particular country or city.
#' 
#' 
#' @param name The name of the segment for the reports.
#' @param segment_id The segment ID of a built in or custom segment e.g. gaid::-3
#' @param user_segment A list of \code{segment_define}'s that apply to users
#' @param session_segment A list of \code{segment_define}'s that apply to sessions
#'
#' @return a segmentFilter object. You can pass a list of these to the request.
#' 
#' @details 
#' \code{segment_ga4} is the top hierarchy of segment creation, for which you will also need:
#' \itemize{
#'  \item \link{segment_define} : AND combination of segmentFilters
#'  \item \link{segment_vector_simple} or \link{segment_vector_sequence}
#'  \item \link{segment_element} that are combined in OR lists for \code{segment_vectors_*}
#' }
#' 
#' 
#' @family v4 segment functions
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
#' 
#' ## make a segment element
#' se <- segment_element("sessions", 
#'                       operator = "GREATER_THAN", 
#'                       type = ""metric"", 
#'                       comparisonValue = 1, 
#'                       scope = "USER")
#'                       
#'                       
#' se2 <- segment_element("medium", 
#'                        operator = "EXACT", 
#'                        type = "dimension", 
#'                        expressions = "organic")
#'                        
#'                        
#' ## choose between segment_vector_simple or segment_vector_sequence
#' ## Elements can be combined into clauses, which can then be 
#' ##    combined into OR filter clauses
#' sv_simple <- segment_vector_simple(list(list(se)))
#' 
#' sv_simple2 <- segment_vector_simple(list(list(se2)))
#' 
#' ## Each segment vector can then be combined into a logical AND
#' 
#' seg_defined <- segment_define(list(sv_simple, sv_simple2))
#' 
#' ## if only one AND definition, you can leave out wrapper list()
#' 
#' seg_defined_one <- segment_define(sv_simple)
#' 
#' ## Each segement defintion can apply to users, sessions or both.
#' ## You can pass a list of several segments
#' 
#' segment4 <- segment_ga4("simple", user_segment = seg_defined)
#' ## Add the segments to the segments param
#' 
#' segment_example <- google_analytics_4(ga_id, 
#'                                       c("2015-07-30","2015-10-01"), 
#'                                      dimensions=c('source','medium','segment'), 
#'                                      segments = segment4, 
#'                                      metrics = c('sessions','bounces')
#'                                      )
#'                                      
#'                                      
#' ## Sequence segment
#' 
#' se2 <- segment_element("medium", 
#'                        operator = "EXACT", 
#'                        type = "dimension", 
#'                        expressions = "organic")
#'                        
#' se3 <- segment_element("medium",
#'                        operator = "EXACT",
#'                        type = "dimension",
#'                        not = TRUE,
#'                       expressions = "organic")
#'                       
#' ## step sequence
#' ## users who arrived via organic then via referral
#' sv_sequence <- segment_vector_sequence(list(list(se2), 
#'                                             list(se3)))
#'                                             
#' seq_defined2 <- segment_define(list(sv_sequence))
#' 
#' segment4_seq <- segment_ga4("sequence", user_segment = seq_defined2)
#' 
#' ## Add the segments to the segments param
#' 
#' segment_seq_example <- google_analytics_4(ga_id, 
#'                                          c("2016-04-01","2016-05-01"), 
#'                                          dimensions=c('source','segment'), 
#'                                          segments = segment4_seq,
#'                                          metrics = c('sessions','bounces')
#'                                          )
#'                                      
#' }
#' 
#' @export
segment_ga4 <- function(name,
                        segment_id=NULL,
                        user_segment=NULL,
                        session_segment=NULL){

  assertthat::assert_that(is.character(name))
  
  if(!is.null(segment_id)){
    assertthat::assert_that(is.character(segment_id),
                            length(segment_id) == 1)
  }

  
  if(!is.null(segment_id)){
    
    if(grepl("^(sessions|users)",segment_id)){
      myMessage("Segment v3 dynamic segment", level=1)
    } else if (!grepl("^(users|gaid)::", segment_id)){
      segment_id <- paste0("gaid::",segment_id)
      myMessage("segment_id passed but doesn't begin with gaid:: - adding it: ", segment_id, level = 3)
    }
    
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
#' \code{segment_define} is in the hierarchy of segment creation, for which you will also need:
#' \itemize{
#'  \item \link{segment_define} : AND combination of segmentFilters
#'  \item \link{segment_vector_simple} or \link{segment_vector_sequence}
#'  \item \link{segment_element} that are combined in OR lists for \code{segment_vectors_*}
#' }
#' 
#' @param segment_filters A list of \link{segment_vector_simple} and \link{segment_vector_sequence}
#' @param not_vector Boolean applied to each segmentFilter step. 
#'   If NULL, assumed FALSE
#' 
#' @return segmentDefinition object for \link{segment_ga4}
#' @family v4 segment functions
#' @export
segment_define <- function(segment_filters,
                           not_vector=NULL){
  
  segment_filters <- unitToList(segment_filters)
  
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
#' \code{segment_vector_simple} is in the hierarchy of segment creation, for which you will also need:
#' \itemize{
#'  \item \link{segment_define} : AND combination of segmentFilters
#'  \item \link{segment_vector_simple} or \link{segment_vector_sequence}
#'  \item \link{segment_element} that are combined in OR lists for \code{segment_vectors_*}
#' }
#' 
#' @param segment_elements A list of OR lists of \link{segment_element}
#' 
#' @return A segment vector you can put in a list for use in \link{segment_ga4}
#' @family v4 segment functions
#' @export
segment_vector_simple <- function(segment_elements){
  
  assertthat::assert_that(is.list(segment_elements))
  
  orFiltersList <- makeOrFilters(segment_elements)
  
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
#' 
#' \code{segment_vector_sequence} is in the hierarchy of segment creation, for which you will also need:
#' \itemize{
#'  \item \link{segment_define} : AND combination of segmentFilters
#'  \item \link{segment_vector_simple} or \link{segment_vector_sequence}
#'  \item \link{segment_element} that are combined in OR lists for \code{segment_vectors_*}
#' }
#' 
#' @param segment_elements a list of OR lists of segment elements
#' @param firstStepMatch FALSE default
#' 
#' @family v4 segment functions   
#' @export
segment_vector_sequence <- function(segment_elements,
                                    firstStepMatch=FALSE){
  
  stepMatchList <- lapply(segment_elements, function(x) attr(x, "matchType"))
  if(!is.null(stepMatchList)){
    assertthat::assert_that(length(segment_elements) == length(stepMatchList))
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
#' \code{segment_element} is the lowest hierarchy of segment creation, for which you will also need:
#' \itemize{
#'  \item \link{segment_define} : AND combination of segmentFilters
#'  \item \link{segment_vector_simple} or \link{segment_vector_sequence}
#'  \item \link{segment_element} that are combined in OR lists for \code{segment_vectors_*}
#' }
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
#' @return An SegmentFilterClause object
#' 
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
                            type = c("METRIC", "DIMENSION"),
                            not = FALSE,
                            expressions=NULL,
                            caseSensitive=NULL,
                            minComparisonValue=NULL,
                            maxComparisonValue=NULL,
                            scope=c("SESSION","USER","HIT","PRODUCT"),
                            comparisonValue=NULL,
                            matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){

  assertthat::assert_that(is.character(name))
  operator <- match.arg(operator)
  type <- match.arg(type)
  assertthat::assert_that(is.logical(not))
  expect_null_or_type(expressions, "character")
  expect_null_or_type(caseSensitive, "logical")  
  expect_null_or_type(minComparisonValue, "numeric")  
  expect_null_or_type(maxComparisonValue, "numeric")  
  scope <- match.arg(scope)
  expect_null_or_type(comparisonValue, "numeric")  
  matchType <- match.arg(matchType)
  
  name <- sapply(name, checkPrefix, prefix = "ga")
  
  if(type == "METRIC"){
    
    if(!operator %in% c("LESS_THAN","GREATER_THAN","EQUAL","BETWEEN")){
      stop('You have the wrong operator, 
           must be one of ("LESS_THAN","GREATER_THAN","EQUAL","BETWEEN")')
    }
    
    smf <- segmentMetricFilter(name = name,
                              scope = scope,
                              operator = operator,
                              comparisonValue = as.character(comparisonValue),
                              maxComparisonValue = as.character(maxComparisonValue))
    
    assertthat::assert_that(inherits(smf, "segmentMetFilter_ga4"))
    
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
    
    assertthat::assert_that(inherits(sdf, "segmentDimFilter_ga4"))
    sfc <- segmentFilterClause(not = not, dimensionFilter = sdf)
    
  }
  
  ## put matchType for sequence segments as an attr
  attr(sfc, "matchType") <- matchType
  sfc
  
}

#' Segment objects
#'
#' @param dynamicSegment A v4 segment object
#' @param segmentId A v3 segment string
#'
#' @return a list of class \code{segment_ga4}
#'
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
#' @keywords internal
dynamicSegment <- function(name, userSegment, sessionSegment){

  assertthat::assert_that(is.character(name))
  
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
#' @keywords internal
segmentDefinition <- function(segmentFilterList){
  
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
#' @keywords internal
segmentFilter <- function(not=FALSE, simpleSegment=NULL, sequenceSegment=NULL){
  
  assertthat::assert_that(is.logical(not))
  
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
#' @keywords internal
simpleSegment <- function(orFiltersForSegmentList){
  
  structure(
    list(
      orFiltersForSegment = orFiltersForSegmentList
    ),
    class = "simpleSegment_ga4"
  )
  
}

#' orFiltersForSegment
#'
#' @keywords internal
orFiltersForSegment <- function(segmentFilterClauseList){
  
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
#' @keywords internal
sequenceSegment <- function(segmentSequenceStepList, firstStepMatch=FALSE){
  
  assertthat::assert_that(is.logical(firstStepMatch))
  
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
#' @param matchType Should it precede or immediately precede the next step
#' 
#' @return segmentSequenceStep object
#' 
#' @keywords internal
segmentSequenceStep <- function(orFiltersForSegmentList,
                                matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){

  matchType <- match.arg(matchType)
  
  structure(
    list(
      orFiltersForSegment = orFiltersForSegmentList,
      matchType = matchType
    ),
    class = "segmentSequenceStep_ga4"
  )
  
}

