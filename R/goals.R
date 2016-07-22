#' List goals
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return Goal list
#' @importFrom googleAuthR gar_api_generator
#' @export
ga_goals_list <- function(accountId,
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