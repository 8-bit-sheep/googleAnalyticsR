#' Experiments Meta data
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param experimentId Experiment Id
#'
#' @return Experiment Meta Data
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_experiment <- function(accountId,
                          webPropertyId,
                          profileId,
                          experimentId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  experiments <- gar_api_generator(url,
                                   "GET",
                                   path_args = list(
                                     accounts = accountId,
                                     webproperties = webPropertyId,
                                     profiles = profileId,
                                     experiments = experimentId
                                   ),
                                   data_parse_function = function(x) x)
  
  experiments()
  
}

#' List Experiments
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return Experiments List
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_experiment_list <- function(accountId,
                               webPropertyId,
                               profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  experiments <- gar_api_generator(url,
                                   "GET",
                                   path_args = list(
                                     accounts = accountId,
                                     webproperties = webPropertyId,
                                     profiles = profileId,
                                     experiments = ""
                                   ),
                                   data_parse_function = function(x) x)
  
  experiments()
  
}