#' Cohort Group object
#' 
#' @param cohorts A list of cohort objects
#' @param lifetimeValue Boolean. Has to be an app. 
#'
#' @family v4 cohort functions
cohortGroup <- function(cohorts,
                        lifetimeValue=FALSE){
  
  testthat::expect_type(cohorts, "list")
  
  structure(
    list(
      cohorts = cohorts,
      lifetimeValue=lifetimeValue
    ),
    class = "cohortGroup_ga4"
  )
  
}

#' Cohort object
#' 
#' @param name A unique name for the cohort
#' @param type Type of cohort.  Only FIRST_VISIT_DATE is support at the moment. 
#' @param dateRange A \link{date_ga4} object
#'
#' @family v4 cohort functions
cohort <- function(name,
                   type = c("FIRST_VISIT_DATE"),
                   dateRange){
  
  type <- match.arg(type)
  
  testthat::expect_type(name, "character")
  testthat::expect_type(dateRange, "date_ga4")
  
  structure(
    list(
      name = name,
      type = type,
      dateRange = dateRange
    ),
    class = "cohort_ga4"
  )
  
}
