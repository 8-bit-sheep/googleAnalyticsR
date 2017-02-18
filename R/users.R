#' List Users
#' 
#' Will list users on an account, webproperty or view level
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id - leave NULL to fetch account level only
#' @param viewId viewId - leave NULL to fetch webProperty level only
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_users_list <- function(accountId,
                          webPropertyId = NULL,
                          viewId = NULL){
  
  url <- sprintf("https://www.googleapis.com/analytics/v3/management/accounts/%s/entityUserLinks",
                 accountId)
  
  if(!is.null(webPropertyId)){
    url <- sprintf("https://www.googleapis.com/analytics/v3/management/accounts/%s/webproperties/%s/entityUserLinks",
                   accountId, webPropertyId)
  }
  
  if(!is.null(viewId)){
    url <- sprintf("https://www.googleapis.com/analytics/v3/management/accounts/%s/webproperties/%s/profiles/%s/entityUserLinks",
                   accountId, webPropertyId, viewId)
  }
  
  users <- gar_api_generator(url,
                             "GET",
                             data_parse_function = function(x) x)
  
  users()
  
}