#' Make a segment object for use
#'
#' @param name The name of the segment for the reports.
#' @param segment_filters A vector of \code{segment_filter}.
#' @param exclude Should this match or exclude the segment defined.
#' @param type Simple or Sequence segment.
#' @param firstStepMatch If a sequence segment, should first step match first hit.
#'
#' @return a segmentFilter object. You can pass a list of these to the request.
#' @family v4 segment functions
#' @export
segment_ga4 <- function(name,
                        segment_filters,
                        exclude=FALSE,
                        type = c("simple","sequence"),
                        firstStepMatch=FALSE){
  type <- match.arg(type)
  
  segment_filters <- as.list(unname(segment_filters))
  
  simple <- NULL
  sequence_seg <- NULL
  
  if(type == "simple"){
    simple <- simpleSegment(segment_filters)
  } else {
    sequence_seg <- sequenceSegment(segment_filters, firstStepMatch)
  }
  
  segmentFilter(matchComplement = exclude,
                simpleSegment = simple,
                sequenceSegment = sequence_seg)
  
}

#' Make a segment filter
#'
#' @param matchType If to be used in sequence segment,
#'   should it precede or immmediately precede
#'
#' @return An orFiltersForSegment object for use in segment_ga4
#' @family v4 segment functions
#' @export
segment_filter <- function(type = c("metric", "dimensions"),
                           matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){
  
  
  
  
  # orFiltersForSegment(..list of segmentfilterclauses...)
  
}
