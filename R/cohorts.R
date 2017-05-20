#' Check the request has cohort metrics
#' 
#' @param metrics A vector of metrics
#' 
#' @family v4 cohort functions
#' @keywords internal
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
#' @keywords internal
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
                    "ga:acquisitionCampaign",
                    "ga:segment")
  
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
#' ## first make a cohort group
#' 
#' cohort4 <- make_cohort_group(list("cohort 1" = c("2015-08-01", "2015-08-01"), 
#'                                   "cohort 2" = c("2015-07-01","2015-07-01")))
#'                                   
#' ## then call cohort report.  No date_range and must include metrics and dimensions
#' ##   from the cohort list
#' cohort_example <- google_analytics_4(ga_id, 
#'                                      dimensions=c('cohort'), 
#'                                      cohort = cohort4, 
#'                                      metrics = c('cohortTotalUsers'))
#'                                      
#' }
#' 
#' @family v4 cohort functions
#' @export
make_cohort_group <- function(cohorts, lifetimeValue=FALSE, cohort_types=NULL){
  
  assertthat::assert_that(is.list(cohorts),
                          is.named(cohorts),
                          is.logical(lifetimeValue))
  
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
#' @keywords internal
cohortGroup <- function(cohorts,
                        lifetimeValue=FALSE){

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
#' @keywords internal
cohort <- function(name,
                   type = c("FIRST_VISIT_DATE"),
                   dateRange){
  
  type <- match.arg(type)
  
  assertthat::assert_that(is.character(name),
                          inherits(dateRange, "date_ga4"))
  
  structure(
    list(
      name = name,
      type = type,
      dateRange = dateRange
    ),
    class = "cohort_ga4"
  )
  
}
