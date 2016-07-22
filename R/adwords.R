#' List AdWords
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return AdWords Links
#' @importFrom googleAuthR gar_api_generator
#' @export
ga_goals_list <- function(accountId,
                          webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  adwords <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               entityAdWordsLinks = ""
                             ))
  
  adwords()
  
}