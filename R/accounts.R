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
#' @family managementAPI functions
#' @export
ga_account_list <- function(){
  
  url <- "https://www.googleapis.com/analytics/v3/management/accountSummaries"
  acc_sum <- gar_api_generator(url,
                               "GET",
                               pars_args = list("start-index"=1),
                               data_parse_function = parse_ga_account_summary)
  
  pages <- gar_api_page(acc_sum, 
                        next_f = paging_function,
                        page_arg = "start-index")
  
  pages
  
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
  acc_sum <- gar_api_generator(url,
                               "GET",
                               pars_args = list("start-index"=1),
                               data_parse_function = function(x) x)
  
  pages <- gar_api_page(acc_sum, 
                        next_f = paging_function,
                        page_arg = "start-index")
  
  pages
}