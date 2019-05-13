#' Get AdWords Link meta data
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param webPropertyAdWordsLinkId AdWords Link Id
#'
#' @return AdWords Meta data
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_adwords <- function(accountId,
                       webPropertyId,
                       webPropertyAdWordsLinkId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  adwords <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 entityAdWordsLinks = webPropertyAdWordsLinkId
                               ),
                               data_parse_function = function(x) x)
  
  adwords()
  
}

#' List AdWords
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return AdWords Links
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_adwords_list <- function(accountId,
                            webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  adwords <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 entityAdWordsLinks = ""
                               ),
                               data_parse_function = parse_ga_adwords_list)
  
  pages <- gar_api_page(adwords, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' @noRd
#' @import assertthat
#' @importFrom dplyr bind_rows select
parse_ga_adwords_list <- function(x){
  
  aaa <- Reduce(bind_rows, x$items$adWordsAccounts)
  o <- x %>% 
    management_api_parsing("analytics#entityAdWordsLinks") %>% 
    cbind(aaa) %>% 
    select(-adWordsAccounts, -entity.webPropertyRef.kind, -entity.webPropertyRef.href, -kind)
  
  if(is.null(o)){
    return(data.frame())
  }
  
  o

}

#' Creates a Google Analytics webProperty-Google Ads link
#'
#'
#' @param adwordsAccountId the customer id of the Adwords account visible within the Adwords account UI on the top right corner -or accessible via the Adwords API
#' @param linkName  a user defined way to call the link between the Adwords and Google Analytics accounts
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#'
#' @description
#'
#' Creates a link between and Adwords (Google ads) account and a Google Analytics property so that Adwords data can be accessed via Google Analytics and vice versa. 
#' 
#'
#' @return confirmation message if successful
#' @family managementAPI functions
#'
#' @importFrom googleAuthR gar_api_generator
#' @export
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/webPropertyAdWordsLinks/insert }{Google documentation}
#' @examples
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' ga_adwords_add_linkid("280-234-7592", "Google Ads Link", "65973592", "UA-65973592-1")
#' }
#' 
ga_adwords_add_linkid <- function(adwordsAccountId, linkName, accountId, webPropertyId) {
  # ensure arguments are all char strings as per Mgment API specs
  adwordsAccountId <- as.character(adwordsAccountId)
  linkName <- as.character(linkName)
  accountId <- as.character(accountId)
  webPropertyId <- as.character(webPropertyId)
  
  
  body <- list(adWordsAccounts = list(list(customerId = adwordsAccountId)), name = linkName)
  
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  
  
  
  f <- gar_api_generator(
    url,
    "POST",
    path_args = list(
      accounts = accountId,
      webproperties = webPropertyId,
      entityAdWordsLinks = ""
    ),
    data_parse_function = function(x) x )
  
  res <- f(the_body = body)
  
  
  
  if (res$kind != "analytics#entityAdWordsLink") {
    stop("Didn't add webProperty-Adwords link for adWords account id ", adwordsAccountId)
  }
  
  myMessage(sprintf(
    "Successfully added link between Adwords account %s and Google Analytics account Id %s - webProperty Id %s with linkId: %s",
    adwordsAccountId, accountId, webPropertyId,
    res$id), 
    level = 3)
}


#'
#' Deletes a Google Analytics webProperty-Google Ads link
#'
#' @param webPropertyAdWordsLinkId   webPropertyAdWordsLinkId
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#'
#' @description
#'
#' Removes a link between and Adwords (Google ads) account and a Google Analytics property
#'
#' @return HTTP Status Code 204 with empty response body, if successful
#' @family managementAPI functions
#'
#' @importFrom googleAuthR gar_api_generator
#' @export
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/webPropertyAdWordsLinks/delete }{Google documentation}
#' @examples
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' # get the ID of the Adwords- Google Analytics link that you want to delete (it's the ID corresponding to the webPropertyAdWordsLinkId field)
#' ga_adwords_list(65973592, "UA-65973592-1") 
#'  
#' ga_adwords_delete_linkid(65973592, "UA-65973592-1", "ezW2dyaiQcGheWRAo69nCw")
#' 
#' 
#' # check its gone
#' ga_adwords_list(65973592, "UA-65973592-1")
#' }
#' 
ga_adwords_delete_linkid <- function(accountId,
                                     webPropertyId,
                                     webPropertyAdWordsLinkId) {
  
  # ensure arguments are all char strings as per Mgment API specs
  
  accountId <- as.character(accountId)
  webPropertyId <- as.character(webPropertyId)
  webPropertyAdWordsLinkId <- as.character(webPropertyAdWordsLinkId)
  
  ga_adwords_list <- ga_adwords_list(accountId,
                                     webPropertyId)
  
  if (!(webPropertyAdWordsLinkId %in% ga_adwords_list$id)) {
    stop(sprintf("webPropertyAdWordsLinkId:  %s not found",
                 webPropertyAdWordsLinkId),call. = FALSE)
  }
  
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  
  f <- gar_api_generator(url,
                         "DELETE",
                         path_args = list(
                           accounts = accountId,
                           webproperties = webPropertyId,
                           entityAdWordsLinks = webPropertyAdWordsLinkId
                         )
  )
  
  res <- suppressWarnings(f())
  if (res$status_code == 204) {
    myMessage("Successfully deleted webPropertyAdWordsLinkId: ", webPropertyAdWordsLinkId, level = 3)
  }
}






