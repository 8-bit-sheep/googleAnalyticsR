#' Check the request has cohort metrics
#' 
#' @param metrics A vector of metrics
#' @family v4 cohort functions
cohort_metric_check <- function(metrics){
  
  metrics <- sapply(metrics, checkPrefix, prefix="ga", USE.NAMES = F)
  allowed_metrics <- c("ga:cohortActiveUsers",
                       "ga:cohortTotalUsers",
                       "ga:cohortAppviewsPerUser",
                       "ga:cohortGoalCompletionsPerUser",
                       "ga:cohortPageviewsPerUser",
                       "ga:cohortRetentionRate",
                       "ga:cohortRevenuePerUser",
                       "ga:cohortVisitDurationPerUser",
                       "ga:cohortSessionsPerUser")
  
  if(all(metrics %in% allowed_metrics)){
    out <- TRUE
  } else {
    stop(metrics, " not in cohort metric list: ", allowed_metrics)
    out <- FALSE
  }
  
  out
  
}

#' Check the request has cohort dimensions
#' 
#' @param metrics A vector of dimensions
#' @family v4 cohort functions
cohort_dimension_check <- function(dimensions){
  
  dimensions <- sapply(dimensions, checkPrefix, prefix="ga", USE.NAMES = F)
  
  allowed_dims <- c("ga:cohort",
                    "ga:cohortNthDay",
                    "ga:cohortNthMonth",
                    "ga:cohortNthWeek",
                    "ga:acquisitionTrafficChannel",
                    "ga:acquisitionSource",
                    "ga:acquisitionMedium",
                    "ga:acquisitionSourceMedium",
                    "ga:acquisitionCampaign")
  
  if(all(dimensions %in% allowed_dims)){
    out <- TRUE
  } else {
    stop(dimensions, " not in cohort metric list: ", allowed_dims)
    out <- FALSE
  }
  
  out
  
  
}

#' Create a cohort group
#' 
#' @param cohorts A named list of start/end date pairs
#' @param lifetimeValue lifetimeValue TRUE or FALSE.  Only works for webapps.
#' @param cohort_types placeholder, does nothing as only FIRST_VISIT_DATE supported.
#' 
#' @details
#' 
#' Example: \code{list("cohort 1" = c("2015-08-01", "2015-08-01"), 
#'                     "cohort 2" = c("2015-07-01","2015-07-01"))}
#' 
#' @return A cohortGroup object
#' 
#' @seealso \url{https://developers.google.com/analytics/devguides/reporting/core/v4/advanced#cohort_and_lifetime_value_ltv_dimensions_and_metrics}
#' 
#' 
#' @family v4 cohort functions
#' @export
makeCohortGroup <- function(cohorts, lifetimeValue=FALSE, cohort_types=NULL){
  
  testthat::expect_type(cohorts, "list")
  testthat::expect_named(cohorts)
  
  cs <- lapply(names(cohorts), function(x) {
    cohort(name = x, dateRange = date_ga4(cohorts[[x]]))
    })
  
  cohortGroup(cs, lifetimeValue=lifetimeValue)
  
}



#' Cohort Group object
#' 
#' @param cohorts A list of cohort objects
#' @param lifetimeValue Boolean. Has to be an app. 
#'
#' @family v4 cohort functions
cohortGroup <- function(cohorts,
                        lifetimeValue=FALSE){
  

  expect_list_of_this(cohorts, "cohort_ga4")

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
  testthat::expect_s3_class(dateRange, "date_ga4")
  
  structure(
    list(
      name = name,
      type = type,
      dateRange = dateRange
    ),
    class = "cohort_ga4"
  )
  
}
