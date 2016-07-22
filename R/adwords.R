#' List AdWords
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return AdWords Links
#' @importFrom googleAuthR gar_api_generator
#' @export
ga_adwords <- function(accountId,
                       webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  adwords <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               entityAdWordsLinks = ""
                             ),
                             data_parse_function = function(x) x)
  
  adwords()
  
}