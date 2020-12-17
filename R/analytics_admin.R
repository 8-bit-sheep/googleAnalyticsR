#' Returns summaries of all accounts accessible by the caller.
#' 
#' 
#' @seealso \href{http://code.google.com/apis/analytics/docs/mgmt/home.html}{Google Documentation}
#' 
#' @details 
#' 
#' @importFrom googleAuthR gar_api_generator
#' @noRd
ga_admin_account_summary <- function(){
  
  url <- "https://analyticsadmin.googleapis.com/v1alpha/accountSummaries"
  # analyticsadmin.accountSummaries.list
  pars = list(pageSize = 50, pageToken = "")
  
  f <- gar_api_generator(url, "GET", pars_args = rmNullObs(pars), 
                         data_parse_function = parse_admin_account_summary)
  
  pages <- gar_api_page(f,
                        page_f = function(x) attr(x, "nextPageToken"),
                        page_method = "param",
                        page_arg = "pageToken")
  
  Reduce(bind_rows, pages)
}

parse_admin_account_summary <- function(x){
  o <- x$accountSummaries
  names(o) <- c("resource_name","account","account_name","property")
  o <- o %>% 
    unnest("property") %>%
    mutate(accountId = gsub("^accounts/","",account),
           propertyId = gsub("^properties/","",property),
           property_name = displayName) %>%
    select(account_name, accountId, property_name, propertyId)
  attr(o, "nextPageToken") <- x$nextPageToken
  o
}

