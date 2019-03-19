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
#' @examples 
#' 
#' \dontrun{
#' 
#'     # get data.frame of unsampled reports you have available
#'     unsample_list <- ga_unsampled_list(accountId = "12345", 
#'                                        webPropertyId = "UA-12345-4", 
#'                                        profileId = "129371234")
#'                                        
#'     # loop through unsampled reports and download as a list of data.frames
#'     dl <- lapply(unsample_list$title, ga_unsampled_download, 
#'                  accountId = "12345", 
#'                  webPropertyId = "UA-12345-4", 
#'                  profileId = "129371234", 
#'                  downloadFile = FALSE)
#'                  
#'     # inspect first data.frame
#'     dl[[1]]
#'     
#'     # download unsampled report to csv file
#'     ga_unsampled_download("my_report_title", 
#'                           accountId = "12345", 
#'                           webPropertyId = "UA-12345-4", 
#'                           profileId = "129371234")
#' 
#' }
#' 
#' 
#' 
#' @importFrom googleAuthR gar_api_generator
#' @importFrom purrr map
#' @importFrom magrittr %>% 
#' @family managementAPI functions
#' @export
#' @import assertthat
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
                                 data_parse_function = parse_unsampled_list)
  
  pages <- gar_api_page(unsampled, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' @noRd
#' @import assertthat
parse_unsampled_list <- function(x){
  
  o <- x %>% 
    management_api_parsing("analytics#unsampledReports") 
  
  if(is.null(o)){
    return(data.frame())
  }
  
  o

}

#' Download Unsampled Report from Google Drive
#'
#' @param reportTitle Title of Unsampled Report (case-sensitive)
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param downloadFile Default TRUE, whether to download, if FALSE returns a dataframe instead

#' @return file location if \code{downloadFile} is TRUE, else a \code{data.frame} of download

#' @examples 
#' 
#' \dontrun{
#' 
#'     # get data.frame of unsampled reports you have available
#'     unsample_list <- ga_unsampled_list(accountId = "12345", 
#'                                        webPropertyId = "UA-12345-4", 
#'                                        profileId = "129371234")
#'                                        
#'     # loop through unsampled reports and download as a list of data.frames
#'     dl <- lapply(unsample_list$title, ga_unsampled_download, 
#'                  accountId = "12345", 
#'                  webPropertyId = "UA-12345-4", 
#'                  profileId = "129371234", 
#'                  downloadFile = FALSE)
#'                  
#'     # inspect first data.frame
#'     dl[[1]]
#'     
#'     # download unsampled report to csv file
#'     ga_unsampled_download("my_report_title", 
#'                           accountId = "12345", 
#'                           webPropertyId = "UA-12345-4", 
#'                           profileId = "129371234")
#' 
#' }
#' @importFrom httr GET add_headers stop_for_status content write_disk progress
#' @importFrom magrittr %>% 
#' @importFrom dplyr filter
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions, googleDriveAPI functions
#' @export
ga_unsampled_download <- function(reportTitle,
                                  accountId,
                                  webPropertyId,
                                  profileId,
                                  downloadFile=TRUE){
  
  # check if proper scope is present
  drive_scope <- "https://www.googleapis.com/auth/drive"
  
  if (!(drive_scope %in% options()$googleAuthR.scopes.selected)) {
    stop(
      sprintf("The %s scope is missing. Please set option and try again.", drive_scope),
      call. = FALSE
    )
  }
  
  unsamps <- ga_unsampled_list(
    accountId = accountId,
    webPropertyId = webPropertyId,
    profileId = profileId
  )
  
  report <- unsamps[unsamps$title == reportTitle, ]
  
  
  if (nrow(report) == 0) {
    stop("Report title not found. Please enter a valid title. 
           Remember it is case-sensitive",
      call. = FALSE
    )
  }
  
  if (nrow(report) > 1) {
    myMessage(sprintf("WARNING: There are multiple reports with the same title of %s. 
                Choosing the most recently created.", reportTitle),
      level = 3
    ) # need to find way to avoid progress bar overwriting
  
    report <- report[report$created == max(report$created), ]
  }
  
  # now there is only 1 report
  if (report$status != "COMPLETED") {
    stop(sprintf("The unsampled report has not COMPLETED. It is currently %s. 
                   Please try again at a later time.", report$status),
      call. = FALSE
    )
  }
  # resolves issue 208
  if (length(report$downloadType) == 0) {
    stop(
      'No download related fields found (downloadType and driveDownloadDetails). 
           Was expecting "GOOGLE_DRIVE" downloadType.',
      call. = FALSE
    )
  }
  
  # https://developers.google.com/analytics/devguides/config/mgmt/v3/unsampled-reports
  if (report$downloadType != "GOOGLE_DRIVE") {
    stop(
      "Only Google Drive download links are currently supported. 
        Contact your Analytics 360 account manager if you would like to change 
        the download location of your unsampled reports.",
      call. = FALSE
    )
  }
  # Get document metadata
  url <- sprintf(
    "https://www.googleapis.com/drive/v2/files/%s",
    toString(report$driveDownloadDetails)
  )
  
  document <- gar_api_generator(url, "GET")()
  
  download_link <- document[["content"]][["webContentLink"]]
  
  # Additional parsing from confirmation page if file is too large
  too_large <- (as.numeric(document[["content"]][["fileSize"]]) / 1048576) >= 25 # bytes to MB  if(too_large){
  
  html <- GET(
    document[["content"]][["webContentLink"]],
    add_headers(Authorization = document[["request"]][["headers"]][["Authorization"]])
  )
  # Read and parse html for confirmation code
  stop_for_status(html)
  too_large_html <- content(html, "text")
  pat <- "&amp;confirm=(.*?)&amp;"
  confirm_code <- regmatches(too_large_html, gregexpr(pat, too_large_html))
  pat <- "&amp;"
  confirm_code <- gsub(pat, "", confirm_code[[1]])
  
  # Final url is in this pattern:
  # https://drive.google.com/a/{company_domain}/uc?export=download&confirm={4character_confirmation_code}&id={documentid}
  pat <- "id=.*download$"
  base_url <- gsub(pat, "", download_link)
  download_link <- paste0(
    base_url, "export=download", "&", confirm_code, "&id=",
    document[["content"]][["id"]]
  )

  # Currently writing with same filename to current working directory
  if (isTRUE(downloadFile)) {
    filename <- sprintf("%s.csv", toString(report$title))
  
    r <- GET(
      download_link,
      add_headers(Authorization = document[["request"]][["headers"]][["Authorization"]]),
      write_disk(filename, overwrite = TRUE),
      progress()
    )
  
    stop_for_status(r)
  
    myMessage(sprintf("%s successfully downloaded!", filename),
      level = 3
    )
    out <- filename
  } else {
    r <- GET(
      download_link,
      add_headers(Authorization = document[["request"]][["headers"]][["Authorization"]])
    )
    stop_for_status(r)
  
    out <- content(r)
  }

out
}