#' Get a remarketing audience
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param remarketingAudienceId The ID of the remarketing audience to retrieve. 
#'
#' @return Remarketing Audience object
#' @seealso \href{https://support.google.com/analytics/answer/2611268?hl=en}{About remarketing audiences}
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_remarketing_get <- function(accountId,
                               webPropertyId,
                               remarketingAudienceId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  rm <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               remarketingAudiences = remarketingAudienceId
                             ),
                             data_parse_function = function(x) x)
  
  rm()
  
}

#' List remarketing audiences
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return Remarketing audience list
#' @seealso \href{https://support.google.com/analytics/answer/2611268?hl=en}{About remarketing audiences}
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_remarketing_list <- function(accountId,
                                webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  rms <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               remarketingAudiences = ""
                             ),
                             data_parse_function = function(x) x)
  
  rms()
  
}

#' Estimate number of users added to the segment yesterday
#' 
#' @param remarketingAudience A remarketing audience object from \link{ga_remarketing_get}
#' 
#' Takes the segment definition from a remarketing audiences and runs 
#'   it against the viewId to see current estimated users
#' 
#' The total audience size is this figure for every membershipDurationDay from yesterday
#'
#' @return data.frame
#' @seealso \href{https://support.google.com/analytics/answer/2611268?hl=en}{About remarketing audiences}
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_remarketing_estimate <- function(remarketingAudience){
  
  stopifnot(remarketingAudience$kind == "analytics#remarketingAudience")
  
  defs <- remarketingAudience$audienceDefinition$includeConditions
  
  yesterday <- Sys.Date() - 1
  users <- google_analytics(
    remarketingAudience$linkedViews,
    start = yesterday - defs$daysToLookBack, end = yesterday,
    metrics = "users",
    segment = defs$segment
  )
}