#' Get hashed version of client id (also known as hashClientId, hashedClientId, or BigQuery's fullVisitorId)
#' 
#' @param webPropertyId Web Property Id
#' @param clientId Client Id
#'
#' @return hashedClientId string
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
hashed_client_id <- function(webPropertyId,
                         clientId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/clientId:hashClientId"
  hashed <- gar_api_generator(url,
                                 "POST",
                                 path_args = list(
                                   webPropertyId = webPropertyId,
                                   clientId = clientId
                                 ),
                                 data_parse_function = function(x) x)
  
  hashed()
}