#' Get single View (Profile)
#' 
#' Gets meta-data for a particular View/Profile
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile (View) Id
#'
#' @return A list of the Views meta-data.
#' @importFrom googleAuthR gar_api_generator
#' @family account structure functions
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' ga_view(1058095, webPropertyId = "UA-1058095-1", profileId = 1855267)
#' }
ga_view <- function(accountId,
                    webPropertyId,
                    profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  views <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               profiles = profileId
                             ),
                             data_parse_function = function(x) x)
  
  views()
  
}

#' List View (Profile)
#' 
#' This gets the meta data associated with the Google Analytics Views for a particular accountId and webPropertyId.  
#' If you want all viewId information for all accounts you have access to, use \link{ga_account_list} instead.
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id e.g. \code{UA-12345-1}
#'
#' @return A \code{data.frame} of meta-data for the views
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family account structure functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' views <- ga_view_list(1058095, "UA-1058095-1")
#' }
ga_view_list <- function(accountId,
                         webPropertyId){
  accountId <- as.character(accountId)
  assert_that(is.string(accountId), is.string(webPropertyId))
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  views <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               profiles = ""
                             ),
                             data_parse_function = parse_view_list)
  
  views()
  
}

parse_view_list <- function(x){
  
  o <- x %>% 
    management_api_parsing("analytics#profiles") %>% 
    select(-childLink.type, -childLink.href, -parentLink.type, -parentLink.href) %>% 
    mutate(created = iso8601_to_r(created),
           updated = iso8601_to_r(updated))
  
  attr(o, "nextLink") <- x$nextLink
  o
}