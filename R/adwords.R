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
parse_ga_adwords_list <- function(x){
  assert_that(x$kind == "analytics#entityAdWordsLinks")
  
  o <- x$items %>% 
    super_flatten() %>% 
    select(-kind, -selfLink)
  
  attr(o, "nextLink") <- x$nextLink
  
  o
  
}
