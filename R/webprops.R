#' List web properties
#' 
#' This gets the meta data for web properties associated with a particular accountId. 
#' If you want all information available to your user, use [ga_account_list] instead.
#'
#' @param accountId Account Id
#'
#' @return A `data.frame` of webproperty meta-data
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family account structure functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' aa <- ga_accounts()
#' wp <- ga_webproperty_list(aa$id[1])
#' }
ga_webproperty_list <- function(accountId){
  accountId <- as.character(accountId)
  assert_that(is.string(accountId))
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  web_prop <- gar_api_generator(url,
                                "GET",
                                path_args = list(
                                  accounts = accountId,
                                  webproperties = ""
                                ),
                                data_parse_function = parse_webproperty_list)
  
  web_prop()
  
}

parse_webproperty_list <- function(x){
  
  o <- x %>% 
    management_api_parsing("analytics#webproperties") %>% 
    select(-childLink.type, -childLink.href, -parentLink.type, -parentLink.href) %>% 
    mutate(created = iso8601_to_r(created),
           updated = iso8601_to_r(updated))
  
  attr(o, "nextLink") <- x$nextLink
  o
}

#' Get a web property
#' 
#' Gets metadata for one particular web property
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id e.g. `UA-12345-1`
#'
#' @return webproperty
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family account structure functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' wp <- ga_webproperty(1058095, "UA-1058095-1")
#' }
ga_webproperty <- function(accountId,
                           webPropertyId){
  accountId <- as.character(accountId)
  assert_that(is.string(accountId), is.string(webPropertyId))
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  web_prop <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId
                               ),
                               data_parse_function = function(x) x)
  
  web_prop()
  
}
