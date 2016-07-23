#' Get goal
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param goalId Goal Id
#'
#' @return Goal meta data
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_goal <- function(accountId,
                    webPropertyId,
                    profileId,
                    goalId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  goals <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               profiles = profileId,
                               goals = goalId
                             ),
                             data_parse_function = function(x) x)
  
  goals()
  
}

#' List goals
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return Goal list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_goal_list <- function(accountId,
                         webPropertyId,
                         profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  goals <- gar_api_generator(url,
                                "GET",
                                path_args = list(
                                  accounts = accountId,
                                  webproperties = webPropertyId,
                                  profiles = profileId,
                                  goals = ""
                                ),
                             data_parse_function = function(x) x)
  
  goals()
  
}