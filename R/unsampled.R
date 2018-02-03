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

#' Download Unsampled Report from Google Drive
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param reportTitle Title of Unsampled Report (case-sensitive)
#' @param file filename and location. Default is {reportTitle}.csv in working directory 
#' @param downloadFile Default TRUE, whether to download, if FALSE returns a dataframe instead

#' @return file location if \code{downloadFile} is TRUE, else a \code{data.frame} of download
#' @importFrom httr GET add_headers stop_for_status content write_disk progress
#' @importFrom purrr map
#' @importFrom magrittr %>% 
#' @importFrom dplyr as_data_frame rename filter
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions, googleDriveAPI functions
#' @export
ga_unsampled_download <- function(accountId,
                                  webPropertyId,
                                  profileId,
                                  reportTitle,
                                  file=sprintf("%s.csv", reportTitle),
                                  downloadFile=TRUE){
  
  # check if proper scope is present
  drive_scope <- "https://www.googleapis.com/auth/drive"
  
  if (!(drive_scope %in% options()$googleAuthR.scopes.selected)) {
    stop(
      printf("The %s scope is missing. Please set option and try again.", drive_scope),
      call. = FALSE
    )
  }

  report <- ga_unsampled_list(accountId, webPropertyId, profileId) %>% 
    .$items %>% 
    map(unlist) %>% 
    as_data_frame() %>% 
    rename(documentId=driveDownloadDetails) %>% 
    filter(title==reportTitle)
  
  if(nrow(report) == 0) {
    stop("Report title not found. Please enter a valid title. 
         Remember it is case-sensitive",
         call.=FALSE) 
  }
  
  if(nrow(report) > 1) {
    myMessage("WARNING: There are multiple reports with the same title. 
              Choosing the most recently created.",
              level=3)  #need to find way to avoid progress bar overwriting
    report <- report %>% filter(created==max(created))
  }
  
  #now there is only 1 report
  if(report$status != "COMPLETED") {
    stop(sprintf("The unsampled report has not COMPLETED. It is currently %s. 
                 Please try again at a later time.", report$status),
         call.=FALSE)
  }
  
  if(report$downloadType != "GOOGLE_DRIVE") {
    stop("The downloadType is not a Google Drive link and cannot be downloaded.",
         call.=FALSE) 
  }
  
  #Get document metadata
  url <- sprintf("https://www.googleapis.com/drive/v2/files/%s", report$documentId)
  document <- gar_api_generator(url, 
                                "GET")()
  download_link <- document[["content"]][["webContentLink"]]
  
  # Additional parsing from confirmation page if file is too large
  too_large <- (as.numeric(document[["content"]][["fileSize"]])/1048576) >= 25 #bytes to MB
  
  if(too_large){ 
    
    html <- GET(document[["content"]][["webContentLink"]],
                add_headers(Authorization=document[["request"]][["headers"]][["Authorization"]]))
    #Read and parse html for confirmation code
    stop_for_status(html)
    too_large_html <- content(html, "text")
    pat <- "&amp;confirm=(.*?)&amp;"
    confirm_code <- regmatches(too_large_html, gregexpr(pat, too_large_html))
    pat <- "&amp;"
    confirm_code <- gsub(pat, "", confirm_code[[1]])
    
    #Final url is in this pattern:
    #https://drive.google.com/a/{company_domain}/uc?export=download&confirm={4character_confirmation_code}&id={documentid}
    pat <- "id=.*download$"
    base_url <- gsub(pat, "", download_link)
    download_link <- paste0(base_url, "export=download", "&", confirm_code, "&id=", 
                            document[["content"]][["id"]])
  }
  
  # Currently writing with same filename to current working directory
  if(isTRUE(downloadFile)){ 
    
    r <- GET(download_link,
             add_headers(Authorization=document[["request"]][["headers"]][["Authorization"]]),
             write_disk(file, overwrite=TRUE),
             progress())
    
    stop_for_status(r)
    
    myMessage(sprintf("%s successfully downloaded!", file),
              level=3)
    out <- file
  } else{ 
    r <- GET(download_link,
             add_headers(Authorization=document[["request"]][["headers"]][["Authorization"]]),
             progress())
    stop_for_status(r) 
    
    out <- content(r)
  }
  
  out
}