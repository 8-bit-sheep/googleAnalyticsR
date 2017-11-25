#' Get Unsampled Report Meta Data
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param unsampledReportId Unsampled Report Id
#'
#' @return Unsampled Report Meta Data
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_unsampled <- function(accountId,
                         webPropertyId,
                         profileId,
                         unsampledReportId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  unsampled <- gar_api_generator(url,
                                 "GET",
                                 path_args = list(
                                   accounts = accountId,
                                   webproperties = webPropertyId,
                                   profiles = profileId,
                                   unsampledReports = unsampledReportId
                                 ),
                                 data_parse_function = function(x) x)
  
  unsampled()
  
}

#' List Unsampled Reports
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return Unsampled Reports List
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_unsampled_list <- function(accountId,
                              webPropertyId,
                              profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  unsampled <- gar_api_generator(url,
                                 "GET",
                                 path_args = list(
                                   accounts = accountId,
                                   webproperties = webPropertyId,
                                   profiles = profileId,
                                   unsampledReports = ""
                                 ),
                                 data_parse_function = function(x) x)
  
  unsampled()
  
}