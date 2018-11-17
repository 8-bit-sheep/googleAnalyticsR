# makes an object class profileFilterLink
as.profileFilterLink <- function(x){
  assertthat::assert_that(
    x$kind == "analytics#profileFilterLink"
  )
  
  structure(
    x,
    class = c("ga_profileFilterLink","list")
  )
}
# test if it is this class
is.profileFilterLink <- function(x){
  inherits(x, "ga_profileFilterLink")
}

# make a filter object
as.filterManagement <- function(x){
  assertthat::assert_that(
    x$kind == "analytics#filter"
  )
  
  structure(
    x,
    class = c("ga_filterManagement","list")
  )
}

is.filterManagement <- function(x){
  inherits(x, "ga_filterManagement")
}


#' List filters for view (profile)
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param viewId Profile Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_view_list <- function(accountId,
                                webPropertyId,
                                viewId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 profiles = viewId,
                                 profileFilterLinks = ""
                               ),
                               data_parse_function = parse_ga_filter_view_list)
  
  pages <- gar_api_page(filters, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' @noRd
#' @import assertthat
parse_ga_filter_view_list <- function(x){
  assert_that(x$kind == "analytics#profileFilterLinks")
  
  o <- x$items %>% 
    super_flatten() %>% 
    select(-kind, -selfLink)
  
  attr(o, "nextLink") <- x$nextLink
  
  o
}

#' Get specific filter for view (profile)
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param viewId Profile Id
#' @param linkId Link Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_view <- function(accountId,
                           webPropertyId,
                           viewId,
                           linkId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 profiles = viewId,
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
  
  pages <- gar_api_page(filters)
  
  pages
  
}

#' Delete a filter from account or remove from view.
#'
#' @param accountId Account Id of the account that contains the filter
#' @param webPropertyId Property Id of the property that contains the filter
#' @param viewId View Id of the view that contains the filter
#' @param filterId Filter Id of the filter to be deleted
#' @param removeFromView Default if FALSE. If TRUE, deletes the filter from the view
#' 
#' @return TRUE if successful
#'
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family managementAPI functions
#' @export
ga_filter_delete <- function(accountId, 
                             webPropertyId = NULL, 
                             viewId = NULL, 
                             filterId, 
                             removeFromView = FALSE) {
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  if(removeFromView){
    
    assert_that(
      !is.null(webPropertyId),
      !is.null(viewId)
    )
    
    f <- gar_api_generator(url,
                           "DELETE",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             profiles = viewId,
                             profileFilterLinks = paste(viewId,filterId, sep=":")
                           ),
                           data_parse_function = function(x) x)
    out <- tryCatch(f(), 
                    warning = function(ex){
                      if(grepl("No JSON content detected",ex)){
                        return(TRUE)
                      } else {
                        return(ex)
                      }
                    }) 
  } else {
    
    f <- gar_api_generator(url,
                           "DELETE",
                           path_args = list(
                             accounts = accountId,
                             filters = filterId
                           ),
                           data_parse_function = function(x) x)
    out <- tryCatch(f(), 
                    warning = function(ex){
                      if(grepl("No JSON content detected",ex)){
                        return(TRUE)
                      } else {
                        return(ex)
                      }
                    }) 
  }
  
  out
}


#' Create a new filter and add it to the view (optional).
#' 
#' Take a filter object and add and/or apply it so its live.
#'
#' @param Filter The Filter object to be added to the account or view.  See examples.
#' @param accountId Account Id of the account to add the Filter to
#' @param webPropertyId Property Id of the property to add the Filter to
#' @param viewId View Id of the view to add the Filter to
#' @param linkFilter If TRUE will apply the Filter to the view. Needs propetyId and viewId to be set.
#' 
#' @return The filterId created if \code{linkFilter=FALSE} or a Filter object if \code{linkFilter=TRUE}
#' 
#' @seealso \url{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/#Filters}
#' 
#' @details 
#' 
#' If you don't set \code{linkFilter=TRUE} then the filter will only be created but not applied.  
#' You will find it listed in the admin panel Account > All Filters.  You can then use \link{ga_filter_apply_to_view} to apply later on. 
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
#' # create and add the filter to the view specified      
#' my_filter <- ga_filter_add(Filter, 
#'                            accountId = 12345, 
#'                            webPropertyId = "UA-12345-1", 
#'                            viewId = 654321,
#'                            linkFilter = TRUE)
#' 
#' # only create the filter, don't apply it to any view - returns filterId for use later
#' my_filter <- ga_filter_add(Filter, 
#'                            accountId = 12345, 
#'                            linkFilter = FALSE)                          
#' 
#' ## Other examples of filters you can create below:
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
#' @import assertthat
#' @family managementAPI functions
#' @export
ga_filter_add <- function(Filter, 
                          accountId, 
                          webPropertyId = NULL, 
                          viewId = NULL, 
                          linkFilter = FALSE) {
  
  assert_that(is.list(Filter),
              is.flag(linkFilter))
  
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
    
    assert_that(
      !is.null(webPropertyId),
      !is.null(viewId)
    )
    
    out <- ga_filter_apply_to_view(filterId, 
                                   accountId = accountId, 
                                   webPropertyId = webPropertyId,
                                   viewId = viewId)
  } else {
    myMessage(sprintf("Created Filter '%s (%s)' but not linked to any view yet.", 
                      Filter$name, filterId), 
              level = 3)
    out <- filterId
  }
  
  out
}


#' Updates an existing filter.
#'
#' @param Filter The Filter object to be updated
#' See examples from ga_filter_add()
#' @param accountId Account Id of the account that contains the filter
#' @param filterId The id of the filter to be modified
#' @param method PUT by default. For patch semantics use PATCH
#' 
#' @return A filterManagement object
#' @seealso \url{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/#Filters} 
#' @examples 
#' 
#' \dontrun{
#' 
#' # create a filter object
#' Filter <- list(
#'     name = 'googleAnalyticsR test1: Exclude Internal Traffic',
#'     type = 'EXCLUDE',
#'     excludeDetails = list(
#'                       field = 'GEO_IP_ADDRESS',
#'                       matchType = 'EQUAL',
#'                       expressionValue = '199.04.123.1',
#'                       caseSensitive = 'False'
#'                       )
#'                  )
#'  # add a filter (but don't link to a View)               
#'  filterId <- ga_filter_add(Filter, 
#'                            accountId = 123456, 
#'                            linkFilter = FALSE)
#'  
#'  # change the name of the filter                    
#'  change_name <- "googleAnalyticsR test2: Changed name via PATCH"
#'  
#'  # using PATCH semantics, only need to construct what you want to change
#'  filter_to_update <- list(name = test_name)
#'  
#'  # update the filter using the filterId 
#'  ga_filter_update(filter_to_update, accountId2, filterId, method = "PATCH")
#' 
#' }
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
  
  o <- f(the_body = Filter)
  
  myMessage(sprintf("Updated filter %s (%s)",
                    o$name, o$id),
            level = 3)
  
  as.filterManagement(o)
}


#' Apply an existing filter to view.
#'
#' @param filterId The id of the filter to be addedd to profile/view
#' @param accountId Account Id of the account that contains the filter
#' @param webPropertyId Web property Id to create profile filter link for
#' @param viewId Profile/view Id to create profile filter link for
#' 
#' @return A profileFilterLink object
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_apply_to_view <- function(filterId, 
                                    accountId, 
                                    webPropertyId, 
                                    viewId) {
  
  body <- list(filterRef = list(id = filterId))
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         "POST",
                         path_args = list(
                           accounts = accountId,
                           webproperties = webPropertyId,
                           profiles = viewId,
                           profileFilterLinks = ""
                         ),
                         data_parse_function = function(x) x)
  
  o <- f(the_body = body)
  
  myMessage(sprintf("Filter '%s (%s)' applied to view '%s (%s)'", 
                    o$filterRef$name, o$filterRef$id, o$profileRef$name, o$profileRef$id), 
            level = 3)
  
  as.profileFilterLink(o)
}


#' Update an existing profile filter link. Patch semantics supported
#'
#' @param viewFilterLink The profileFilterLink object
#'
#' @param accountId Account Id of the account that contains the filter
#' @param webPropertyId Web property Id to which the profile filter link belongs
#' @param viewId View Id to which the profile filter link belongs
#' @param linkId The id of the profile filter link to be updated
#' @param method PUT by default. Supports patch semantics when set to PATCH
#' 
#' @seealso \url{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/profileFilterLinks}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # create a filter object
#' Filter <- list(
#'  name = 'googleAnalyticsR test: Exclude Internal Traffic',
#'  type = 'EXCLUDE',
#'  excludeDetails = list(
#'    field = 'GEO_IP_ADDRESS',
#'    matchType = 'EQUAL',
#'    expressionValue = '199.04.123.1',
#'    caseSensitive = 'False'
#'    )
#'  )
#'  
#'  # link Filter to a View
#'  response <- ga_filter_add(Filter, 
#'                            accountId = 12345, 
#'                            webPropertyId = "UA-12345-1", 
#'                            viewId = 654321, 
#'                            linkFilter = TRUE)
#'                            
#' # create Filter patch to move existing filter up to rank 1
#' viewFilterLink <- list(rank = 1)
#' 
#' # use the linkId given in response$id to update to new rank 1
#' response2 <- ga_filter_update_filter_link(viewFilterLink, 
#'                                           accountId = 12345, 
#'                                           webPropertyId = "UA-12345-1", 
#'                                           viewId = 654321,  
#'                                           linkId = response$id)
#'
#' }
#'
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_update_filter_link <- function(viewFilterLink, 
                                         accountId, 
                                         webPropertyId, 
                                         viewId, 
                                         linkId, 
                                         method = c("PUT","PATCH")){
  
  method <- match.arg(method)
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         method,
                         path_args = list(
                           accounts = accountId,
                           webproperties = webPropertyId,
                           profiles = viewId,
                           profileFilterLinks = linkId
                         ),
                         data_parse_function = function(x) x)
  
  f(the_body = viewFilterLink)
}
