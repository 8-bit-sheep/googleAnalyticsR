#' List filters for view (profile)
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_view_list <- function(accountId,
                                webPropertyId,
                                profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 profiles = profileId,
                                 profileFilterLinks = ""
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}

#' Get specific filter for view (profile)
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param linkId Link Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_view <- function(accountId,
                           webPropertyId,
                           profileId,
                           linkId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 profiles = profileId,
                                 profileFilterLinks = linkId
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}

#' Get specific filter for account
#'
#' @param accountId Account Id
#' @param filterId Filter Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter <- function(accountId,
                      filterId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 filters = filterId
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}

#' List filters for account
#'
#' @param accountId Account Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_list <- function(accountId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 filters = ""
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}

#' Delete a filter from account or remove from view.
#'
#' @param accountId Account Id of the account that contains the filter
#' @param propertyId Property Id of the property that contains the filter
#' @param viewId View Id of the view that contains the filter
#' @param filterId Filter Id of the filter to be deleted
#' @param removeFromViewOnly Default if FALSE. If TRUE, deletes the filter only from the view
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_delete <- function(accountId, 
                             propertyId, 
                             viewId, 
                             filterId, 
                             removeFromViewOnly = FALSE) {
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  if(removeFromViewOnly){
    
    f <- gar_api_generator(url,
                           "DELETE",
                           path_args = list(
                             accounts = accountId,
                             webproperties = propertyId,
                             profiles = viewId,
                             profileFilterLinks = paste(viewId,filterId, sep=":")
                           ),
                           data_parse_function = function(x) x)
    out <- f()
  } else {
    
    f <- gar_api_generator(url,
                           "DELETE",
                           path_args = list(
                             accounts = accountId,
                             filters = filterId
                           ),
                           data_parse_function = function(x) x)
    out <- f()
  }
  
  out
}


#' Create a new filter and add it to the view (optional).
#'
#' @param Filter The Filter object to be added to the account or view.  See examples.
#' @param accountId Account Id of the account to add the Filter to
#' @param propertyId Property Id of the property to add the Filter to
#' @param viewId View Id of the view to add the Filter to
#' @param linkFilter If TRUE will apply the Filter to the view. Needs propetyId and viewId to be set.
#' 
#' @seealso \url{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/#Filters}
#' 
#' @examples 
#' 
#' \dontrun{
#' ## Create a filter object for adding an IP exclusion:
#' Filter <- list(
#'                name = 'Exclude Internal Traffic',
#'                type = 'EXCLUDE',
#'                excludeDetails = list(
#'                    field = 'GEO_IP_ADDRESS',
#'                    matchType = 'EQUAL',
#'                    expressionValue = '199.04.123.1',
#'                    caseSensitive = 'False'
#'                                     )
#'               )
#'
#' ## Create a filter object for making campaign medium lowercase
#' Filter <- list(
#'                name = 'Lowercase Campaign Medium',
#'                type = 'LOWERCASE',
#'                lowercaseDetails = list(
#'                    field = 'CAMPAIGN_MEDIUM'
#'                                     )
#'               )
#'
#' ## Create a filter object to append hostname to URI
#' Filter <- list(
#'                name = 'Append hostname to URI',
#'                type = 'ADVANCED',
#'                advancedDetails = list(
#'                    fieldA = 'PAGE_HOSTNAME',
#'                    extractA = '(.*)',
#'                    fieldARequired = 'True',
#'                    fieldB = 'PAGE_REQUEST_URI',
#'                    extractB = '(.*)',
#'                    fieldBRequired = 'False',
#'                    outputConstructor = '$A1$B1',
#'                    outputToField = 'PAGE_REQUEST_URI',
#'                    caseSensitive = 'False',
#'                    overrideOutputField = 'True'
#'                                     )
#'               )
#'
#' ## Create a filter object to add www hostname without it
#' Filter <- list(
#'                name = 'Search and Replace www',
#'                type = 'SEARCH_AND_REPLACE',
#'                searchAndReplaceDetails = list(
#'                    field = 'PAGE_HOSTNAME',
#'                    searchString = '^exampleUSA\\.com$',
#'                    replaceString = 'www.exampleUSA.com',
#'                    caseSensitive = 'False'
#'                                     )
#'               )
#' 
#' }
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_add <- function(Filter, 
                          accountId, 
                          propertyId = FALSE, 
                          viewId = FALSE, 
                          linkFilter = FALSE) {
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         "POST",
                         path_args = list(
                           accounts = accountId,
                           filters = ""
                         ),
                         data_parse_function = function(x) x$id)
  
  filterId <- f(the_body = Filter)
  
  if(linkFilter){
    body <- list(filterRef = list(id = filterId))
    url <- "https://www.googleapis.com/analytics/v3/management/"
    f <- gar_api_generator(url,
                           "POST",
                           path_args = list(
                             accounts = accountId,
                             webproperties = propertyId,
                             profiles = viewId,
                             profileFilterLinks = ""
                           ),
                           data_parse_function = function(x) x)
    
    f(the_body = body)
  }
}


#' Updates an existing filter.
#'
#' @param Filter The Filter object to be updated
#' See examples from ga_filter_add()
#' @param accountId Account Id of the account that contains the filter
#' @param filterId The id of the filter to be modified
#' @param method PUT by default. For patch semantics use PATCH
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_update <- function(Filter, 
                             accountId, 
                             filterId, 
                             method = c("PUT","PATCH")){
  method <- match.arg(method)
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         method,
                         path_args = list(
                           accounts = accountId,
                           filters = filterId
                         ),
                         data_parse_function = function(x) x)
  
  f(the_body = Filter)
}


#' Apply an existing filter to view.
#'
#' @param filterId The id of the filter to be addedd to profile/view
#' @param accountId Account Id of the account that contains the filter
#' @param propertyId Web property Id to create profile filter link for
#' @param viewId Profile/view Id to create profile filter link for
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_apply_to_view <- function(filterId, 
                                    accountId, 
                                    propertyId, 
                                    viewId) {
  
  body <- list(filterRef = list(id = filterId))
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         "POST",
                         path_args = list(
                           accounts = accountId,
                           webproperties = propertyId,
                           profiles = viewId,
                           profileFilterLinks = ""
                         ),
                         data_parse_function = function(x) x)
  
  f(the_body = body)
}


#' Update an existing profile filter link. Patch semantics supported
#'
#' @param viewFilterLink The viewFilterLink object
#' ## Example:
#' ## Changing the rank of the filter in the view:
#' viewFilterLink <- list(rank = 4)
#'
#' @param accountId Account Id of the account that contains the filter
#' @param propertyId Web property Id to which the profile filter link belongs
#' @param viewId View Id to which the profile filter link belongs
#' @param linkId The id of the profile filter link to be updated
#' @param method PUT by default. Supports patch semantics when set to PATCH
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_update_filter_link <- function(viewFilterLink, 
                                         accountId, 
                                         propertyId, 
                                         viewId, 
                                         linkId, 
                                         method = c("PUT","PATCH")){
  
  method <- match.arg(method)
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         method,
                         path_args = list(
                           accounts = accountId,
                           webproperties = propertyId,
                           profiles = viewId,
                           profileFilterLinks = linkId
                         ),
                         data_parse_function = function(x) x)
  
  f(the_body = viewFilterLink)
}
