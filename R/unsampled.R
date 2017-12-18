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

#' Download Unsampled Report
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param reportTitle Title of Unsampled Report (case-sensitive)
#' @param file filename and location. Default is {reportTitle}.csv in working directory 
#' @param download Default TRUE, whether to download, if FALSE returns a dataframe instead

#' @return Unsampled Report Downloaded as CSV
#' @importFrom httr GET
#' @family managementAPI functions, googleDriveAPI functions
#' @export

ga_unsampled_download <- function(accountId,
                                  webPropertyId,
                                  profileId,
                                  reportTitle,
                                  file=sprintf("%s.csv", reportTitle),
                                  download=TRUE){
  
  report <- ga_unsampled_list(accountId, webPropertyId, profileId) %>% 
            .$items %>% 
            purrr::map(unlist) %>% 
            dplyr::as_data_frame() %>% 
            dplyr::rename(documentId=driveDownloadDetails) %>% 
            dplyr::filter(title==reportTitle)
  
  if(nrow(report) == 0) {
    cat("Report title not found. Please enter a valid title. Remember it is case-sensitive") 
    stop()
  }
  
  if(nrow(report) >= 1) {
    cat("WARNING: There are multiple reports with the same title. Choosing the most recently created.") 
    report <- report %>% dplyr::filter(created==max(created))
  }
  
  #now there is only 1 report
  if(report$status != "COMPLETED") {
    cat(sprintf("The unsampled report has not COMPLETED. It is currently %s. Please try again at a later time.", report$status)) 
    stop()
  }
  if(report$downloadType != "GOOGLE_DRIVE") {
    cat("The downloadType is not a Google Drive link and cannot be downloaded.") #Is there another valid type?
    stop()
  }
  
  #Get document metadata
  url <- sprintf("https://www.googleapis.com/drive/v2/files/%s", report$documentId)
  document <- gar_api_generator(url, 
                                "GET")
  document <- document()
  
  if(download == TRUE){ # Currently writing with same filename to current working directory
    r <- httr::GET(document[["content"]][["webContentLink"]],
                   httr::add_headers(Authorization=document[["request"]][["headers"]][["Authorization"]]),
                   httr::write_disk(file, overwrite=TRUE, progress()))
    httr::stop_for_status(r)    
  } else{ 
    r <- httr::GET(document[["content"]][["webContentLink"]],
                   httr::add_headers(Authorization=document[["request"]][["headers"]][["Authorization"]]))
    httr::stop_for_status(r) 
    df <- httr::content(r)
  }
}