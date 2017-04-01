#' List Custom Data Sources
#' 
#' Get a list of custom data sources you have configured in Google Analytics web UI.
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' 
#' @details 
#' 
#' You primarily need this to get the \code{customDataSourceId} for the uploads via \link{ga_custom_upload_file}
#'
#' @return Custom Data Source
#' @importFrom googleAuthR gar_api_generator
#' @family custom datasource functions
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
  res <- cds()
  out <- res$items
  attr(out, "meta") <- res[setdiff(names(res),"items")]
  
  structure(out, class = c("ga_custom_datasource","data.frame"))
  
}

#' List Custom Data Source Uploads
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param customDataSourceId Custom data source Id
#'
#' @return Custom Data Source Uploads List
#' @importFrom googleAuthR gar_api_generator
#' @family custom datasource functions
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
  
  out <- cds()
  
  out$items[,c("id","accountId","customDataSourceId","status")]
  
}

#' Custom Data Source Upload Status
#'
#' Get the status of a custom upload
#' 
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param customDataSourceId Custom data source Id
#' @param uploadId upload Id
#' @param upload_object A custom upload Id object. Supply this or the other arguments.
#' 
#' @details
#' 
#' You can supply either \code{upload_object} generated via function or \link{ga_custom_upload_file}, or make an \code{}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' upload_me <- data.frame(medium = "shinyapps", 
#'                         source = "referral", 
#'                         adCost = 1, 
#'                         date = "20160801")
#'                         
#' obj <- ga_custom_upload_file(47850439, 
#'                              "UA-4748043-2", 
#'                              "_jDsJHSFSU-uw038Bh8fUg", 
#'                              upload_me)
#'                              
#' ## obj will initially have status = PENDING
#' obj
#' ==Google Analytics Custom Data Source Upload==
#' Custom Data Source ID:  _jDsJHSFSU-uw038Bh8fUg 
#' Account ID:             47850439 
#' Web Property Id:        UA-4748043-2 
#' Upload ID:              7yHLAkeLSiK1zveVTiWZwA 
#' Status:                 PENDING 
#' 
#' ## Send obj to ga_custom_upload() to check and renew status
#' obj <- ga_custom_upload(upload_object = obj)
#' obj
#' 
#' ==Google Analytics Custom Data Source Upload==
#' Custom Data Source ID:  _jDsJHSFSU-uw038Bh8fUg 
#' Account ID:             47850439 
#' Web Property Id:        UA-4748043-2 
#' Upload ID:              7yHLAkeLSiK1zveVTiWZwA 
#' Status:                 COMPLETED 
#' 
#' }
#'
#' @return An object of class \code{ga_custom_data_source_upload}
#' @importFrom googleAuthR gar_api_generator
#' @family custom datasource functions
#' @export
ga_custom_upload <- function(accountId,
                             webPropertyId,
                             customDataSourceId,
                             uploadId,
                             upload_object){
  
  if(missing(upload_object)){
    if(any(missing(accountId), 
           missing(webPropertyId), 
           missing(customDataSourceId),
           missing(uploadId))){
      stop("Must supply one of upload_object or all other arguments")
    }
  } else {
    accountId <- upload_object$accountId
    webPropertyId <- upload_object$webPropertyId
    customDataSourceId <- upload_object$customDataSourceId
    uploadId <- upload_object$id
  }
  
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
  
  out <- cds()
  
  structure(c(out,webPropertyId = webPropertyId), 
            class = "ga_custom_data_source_upload")
  
}

#' Upload data to Google Analytics
#' 
#' Upload external data up to 1GB to Google Analytics via the management API.
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param customDataSourceId Custom data source Id
#' @param upload An R data.frame or a file path location (character)
#' 
#' @details 
#' 
#' You need to create a custom data source in the web UI first.  
#' 
#' If you are uploading an R data frame, the function will prefix the column names with \code{"ga:"} for you if necessary.
#'   
#' After upload check the status by querying data sources using \link{ga_custom_upload} 
#'   and examining the \code{status} field.
#'
#' Currently only supports simple uploads (not resumable).
#' 
#' @seealso 
#' 
#' A guide for preparing the data is available: \href{https://developers.google.com/analytics/solutions/data-import-cost}{from Google here}.
#' 
#' The dev guide for this function: \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/data-import#format}{Data Import Developer Guide}
#' 
#'
#' @return An object of class \code{ga_custom_data_source_upload}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' upload_me <- data.frame(medium = "shinyapps", 
#'                         source = "referral", 
#'                         adCost = 1, 
#'                         date = "20160801")
#'                         
#' obj <- ga_custom_upload_file(47850439, 
#'                              "UA-4748043-2", 
#'                              "_jDsJHSFSU-uw038Bh8fUg", 
#'                              upload_me)
#'                              
#' ## obj will initially have status = PENDING
#' obj
#' ==Google Analytics Custom Data Source Upload==
#' Custom Data Source ID:  _jDsJHSFSU-uw038Bh8fUg 
#' Account ID:             47850439 
#' Web Property Id:        UA-4748043-2 
#' Upload ID:              7yHLAkeLSiK1zveVTiWZwA 
#' Status:                 PENDING 
#' 
#' ## Send obj to ga_custom_upload() to check and renew status
#' obj <- ga_custom_upload(upload_object = obj)
#' obj
#' 
#' ==Google Analytics Custom Data Source Upload==
#' Custom Data Source ID:  _jDsJHSFSU-uw038Bh8fUg 
#' Account ID:             47850439 
#' Web Property Id:        UA-4748043-2 
#' Upload ID:              7yHLAkeLSiK1zveVTiWZwA 
#' Status:                 COMPLETED 
#' 
#' }
#' 
#' 
#' @importFrom googleAuthR gar_api_generator
#' @importFrom utils write.csv
#' @family custom datasource functions
#' @export
ga_custom_upload_file <- function(accountId,
                                  webPropertyId,
                                  customDataSourceId,
                                  upload){
  
  if(inherits(upload, "data.frame")){
    temp <- tempfile()
    on.exit(unlink(temp))
    
    names(upload) <- vapply(names(upload), checkPrefix, character(1))
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
  
  ## from https://github.com/MarkEdmondson1234/googleAnalyticsR/issues/40#issuecomment-258287829
  ## to get name of file in data uploads
  # metadata <- tempfile()

  # metadata_content <- jsonlite::toJSON(list(title = jsonlite::unbox(upload_name)))
  
  # writeLines(metadata_content, metadata)

  # upload_me <- list(
  #   metadata = httr::upload_file(metadata, type = "application/json; charset=UTF-8"),
  #   media = httr::upload_file(temp, "application/octet-stream")
  # )
  
  upload_me <- httr::upload_file(temp, "application/octet-stream")
  
  req <- cds(the_body = upload_me)
  
  if(req$status == 200){
    myMessage("File uploaded", level = 3)
  } else {
    myMessage("Problem upload file", level = 3)
  }
  
  res <- httr::content(req, as = "raw")
  
  structure(
    c(res, webPropertyId = webPropertyId), 
    class = "ga_custom_data_source_upload"
  )
}