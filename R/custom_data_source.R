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

#' Get Custom Data Source Data
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

#' Get Custom Data Source Upload Meta data
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

#' Upload data to Google Analytics
#' 
#' Upload external data up to 1GB to Google Analytics via the management API.
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param customDataSourceId Custom data source Id
#' @param upload An R data.frame or a file path location (character).
#' 
#' @details 
#' 
#' You need to create a custom data source in the web UI first.  
#' 
#' If you are uploading an R data frame, the function will prefix the column names with \code{"ga:"} for you if necessary.
#'   
#' After upload check the status by listing the data sources using \link{ga_custom_upload} 
#'   and examining the \code{status} field.
#'
#' Currently only supports simple uploads.
#' 
#' @seealso 
#' 
#' A guide for preparing the data is available \href{from Google here}{https://developers.google.com/analytics/solutions/data-import-cost}.
#' 
#' See also \href{Data Import Developer Guide}{https://developers.google.com/analytics/devguides/config/mgmt/v3/data-import#format}
#' 
#'
#' @return A custom upload Id object
#' @importFrom googleAuthR gar_api_generator
#' @export
ga_custom_upload_file <- function(accountId,
                                  webPropertyId,
                                  customDataSourceId,
                                  upload){
  
  if(inherits(upload, "data.frame")){
    temp <- tempfile()
    on.exit(unlink(temp))
    names(upload) <- vapply(names(upload), checkPrefix, character(1))
    if(!"ga:date" %in% names(upload)){
      stop("Need at least one column with 'date'")
    }
    write.csv(upload, file = temp, row.names = FALSE)
  } else if(inherits(upload, "character")){
    temp <- upload
  } else {
    stop("Unsupported upload, must be a file location or R data.frame, got:", class(upload))
  }
  
  url <- "https://www.googleapis.com/upload/analytics/v3/management/"
  cds <- gar_api_generator(url,
                           "POST",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             customDataSources = customDataSourceId,
                             uploads = ""
                           ),
                           pars_args = list(
                             uploadType = "media"
                           ))
  
  req <- cds(the_body = httr::upload_file(temp, type = "application/octet-stream"))
  
  if(req$status == 200){
    message("File uploaded")
  } else {
    message("Problem upload file")
  }
  
  httr::content(req, as = "raw")
}