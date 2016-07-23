#' Get single View (Profile)
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile (View) Id
#'
#' @return webproperty
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
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
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return webproperty
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_view_list <- function(accountId,
                           webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  views <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               profiles = ""
                             ),
                             data_parse_function = function(x) x)
  
  views()
  
}