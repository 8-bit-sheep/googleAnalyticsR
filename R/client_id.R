#' Get hashed version of client id (also known as hashClientId, hashedClientId, or BigQuery's fullVisitorId)
#' 
#' @param webPropertyId Web Property Id
#' @param clientId Client Id
#'
#' @return hashedClientId object list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_clientid_hash <- function(webPropertyId, clientId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/clientId:hashClientId"
  
  body <- list(
    webPropertyId = webPropertyId,
    clientId = clientId
  )
  
  hashed <- gar_api_generator(url,
    "POST",
    data_parse_function = function(x) x
  )
  
  hashed(the_body = body)
}