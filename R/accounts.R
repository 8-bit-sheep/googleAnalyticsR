#' @rdname ga_account_list
#' @export
google_analytics_account_list <- function(){
  
  .Deprecated("ga_account_list", package = "googleAnalyticsR")
  
  ga_account_list()
}

#' Get account summary including the ViewId
#'
#' @seealso https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountSummaries/list
#'
#' @return a dataframe of all account, webproperty and view data
#' @importFrom googleAuthR gar_api_generator
#' @importFrom dplyr bind_rows
#' @family managementAPI functions
#' @export
ga_account_list <- function(){
  
  url <- "https://www.googleapis.com/analytics/v3/management/accountSummaries"
  acc_sum <- gar_api_generator(url,
                               "GET",
                               data_parse_function = parse_ga_account_summary)
  
  pages <- gar_api_page(acc_sum, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' Get account list 
#'
#'
#' @return a dataframe of accounts
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_accounts <- function(){
  
  url <- "https://www.googleapis.com/analytics/v3/management/accounts"
  accs <- gar_api_generator(url,
                               "GET",
                               data_parse_function = parse_ga_accounts)
  
  pages <- gar_api_page(accs, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)

}

#' @noRd
#' @import assertthat
#' @importFrom dplyr select
parse_ga_accounts <- function(x){
  
  o <- x %>% 
    management_api_parsing("analytics#accounts") %>% 
    select(-childLink.type, -childLink.href) %>% 
    mutate(created = iso8601_to_r(created),
           updated = iso8601_to_r(updated))
  
  attr(o, "nextLink") <- x$nextLink
  o
}