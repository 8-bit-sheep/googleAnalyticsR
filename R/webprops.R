#' List web properties
#'
#' @param accountId Account Id
#'
#' @return webproperty list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_webproperty_list <- function(accountId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  web_prop <- gar_api_generator(url,
                                "GET",
                                path_args = list(
                                  accounts = accountId,
                                  webproperties = ""
                                ),
                                data_parse_function = function(x) x)
  
  web_prop()
  
}

#' Get web property
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return webproperty
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_webproperty <- function(accountId,
                           webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  web_prop <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId
                               ),
                               data_parse_function = function(x) x)
  
  web_prop()
  
}