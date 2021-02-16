#' Account summary for all accounts available to your user
#' 
#' Get a summary of all your accounts, web properties and views your authenticated user can see.
#'
#' @seealso https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountSummaries/list
#' 
#' @param type Whether to get account summary from universal analytics of GA4 (App_Web) properties
#'
#' @return a dataframe of all account, webproperty and view data
#' @importFrom googleAuthR gar_api_generator
#' @importFrom dplyr bind_rows
#' @family account structure functions
#' @export
#' 
#' @description 
#' 
#' This is the recommended way to get all your account details for your user, including the web property and View IDs.  The `$viewId` column contains the ID you need for the data fetching functions such as [google_analytics].
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' al <- ga_account_list()
#' al$viewId
#' 
#' ## get account summary of GA4 properties
#' ga_account_list("ga4")
#' }
ga_account_list <- function(type = c("universal","ga4", "data")){
  type <- match.arg(type)
  
  if(type %in% c("ga4","data")){
    return(ga_admin_account_summary())
  }
  
  url <- "https://www.googleapis.com/analytics/v3/management/accountSummaries"
  acc_sum <- gar_api_generator(url,
                               "GET",
                               data_parse_function = parse_ga_account_summary)
  
  pages <- gar_api_page(acc_sum, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' List account metadata
#' 
#' This gets the meta data associated with the accounts you have access to with your user.  
#' If you want all information such as web properties and viewIds, use [ga_account_list] instead.
#'
#' @importFrom googleAuthR gar_api_generator
#' @family account structure functions
#' 
#' @description 
#' 
#'   This gets a list of account meta data, that can be used in other management API functions.
#'   
#' @return A `data.frame` with accountid, name, an R datetime object (`POSIXct`) when the account was created and last updated, and the effective permissions your user has for those accounts. 
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' ga_accounts()
#' 
#' }
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
