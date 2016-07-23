#' List Custom Data Sources
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return Custom Data Source
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_custom_datasource <- function(accountId,
                                 webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  cds <- gar_api_generator(url,
                           "GET",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             customDataSources = ""
                           ),
                           data_parse_function = function(x) x)
  
  cds()
  
}

#' List Custom Data Uploads
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param customDataSourceId Custom data source Id
#'
#' @return Custom Data Source Uploads List
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_custom_upload_list <- function(accountId,
                                  webPropertyId,
                                  customDataSourceId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  cds <- gar_api_generator(url,
                           "GET",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             customDataSources = customDataSourceId,
                             uploads = ""
                           ),
                           data_parse_function = function(x) x)
  
  cds()
  
}

#' Get Custom Data Upload Meta data
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param customDataSourceId Custom data source Id
#' @param uploadId upload Id
#'
#' @return Custom Data Source Upload Meta data
#' @importFrom googleAuthR gar_api_generator
#' @export
ga_custom_upload <- function(accountId,
                             webPropertyId,
                             customDataSourceId,
                             uploadId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  cds <- gar_api_generator(url,
                           "GET",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             customDataSources = customDataSourceId,
                             uploads = uploadId
                           ),
                           data_parse_function = function(x) x)
  
  cds()
  
}