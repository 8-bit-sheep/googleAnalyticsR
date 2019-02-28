#' List Users
#' 
#' Will list users on an account, webproperty or view level
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id - set to NULL to operate on account level only
#' @param viewId viewId - set to NULL to operate on webProperty level only
#' 
#' @description Get a list of Account level user links, or if you supply the webPropertyId or viewId it will show user links at that level
#' 
#' @return A \code{data.frame} of user entity links including the linkId, email and permissions
#' 
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountUserLinks}{Account User Links Google Documentation}
#'
#' @importFrom googleAuthR gar_api_generator
#' @family User management functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' ga_users_list(47480439)
#' ga_users_list(47480439, webPropertyId = "UA-47480439-2")
#' ga_users_list(47480439, webPropertyId = "UA-47480439-2", viewId = 81416156)
#' 
#' # use NULL to only list linkids for that level
#' ga_users_list(47480439, webPropertyId = NULL, viewId = NULL)
#' }
ga_users_list <- function(accountId,
                          webPropertyId = "~all",
                          viewId = "~all"){
  accountId <- as.character(accountId)
  
  users <- gar_api_generator(make_user_url(accountId, webPropertyId, viewId),
                             "GET",
                             data_parse_function = parse_ga_users_list)
  
  pages <- gar_api_page(users, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' @noRd
#' @import assertthat
#' @importFrom dplyr rename select ends_with
parse_ga_users_list <- function(x){
  
  y <- x %>% 
    management_api_parsing("analytics#entityUserLinks")
  
  if(is.null(y)){
    myMessage("No users found")
    return(data.frame())
  }
  
  y %>% 
    select(-userRef.kind, -ends_with("kind"), -ends_with("href")) %>% 
    rename(linkId = id)
  
}


#' Delete all user access for an email
#' 
#' This deletes a user via their email reference for all webproperties and views for the account given.
#' 
#' @param email The email of the user to delete
#' @param accountId The accountId that the user will be deleted from including all web properties and Views underneath.
#' 
#' @description 
#' This is a wrapper around calls to \link{ga_users_list} and \link{ga_users_delete_linkid}.  If you want more fine-grained control look at those functions.
#' 
#' The user email is deleted from all web properties and views underneath the accountId you provide. 
#' @import assertthat
#' @importFrom dplyr filter select
#' @importFrom purrr map map2 pmap
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountUserLinks/delete}{Google Documentation}
#' @family User management functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' ga_users_delete("brian@agency.com", 12345678)
#' 
#' # multiple emails
#' ga_users_delete(c("brian@agency.com", "bill@benland.com"), 1234567)
#' 
#' }
ga_users_delete <- function(email, accountId){
  default_project_message()
  accountId <- as.character(accountId)
  
  a_lnks <- ga_users_list(accountId, webPropertyId = NULL, viewId = NULL)
  wb_lnks <- ga_users_list(accountId, viewId = NULL)
  view_lnks <- ga_users_list(accountId)
  
  a_li <- a_lnks %>% 
    filter(userRef.email %in% email, permissions.local != "") %>% 
    select(linkId, entity.accountRef.id)
  
  wb_li <- wb_lnks %>% 
    filter(userRef.email %in% email, permissions.local != "") %>% 
    select(linkId, entity.webPropertyRef.id)
  
  view_li <- view_lnks %>% 
    filter(userRef.email %in% email, permissions.local != "") %>% 
    select(linkId, entity.profileRef.webPropertyId, entity.profileRef.id)
  
  if(nrow(a_li) > 0){
    ga_users_delete_linkid(a_li$linkId, accountId = accountId, check = FALSE)
  }
  
  if(nrow(wb_li) > 0){
    web_props <- unique(wb_li$entity.webPropertyRef.id)
    
    lapply(web_props, function(x){
      ids <- wb_li %>% 
        filter(entity.webPropertyRef.id == x) %>% 
        select(linkId)
      
      ga_users_delete_linkid(ids$linkId, 
                             accountId = accountId,
                             webPropertyId = x, 
                             check = FALSE)
    })
    
  }
  
  if(nrow(view_li) > 0){
    views <- unique(view_li$entity.profileRef.id)
    
    lapply(views, function(x){
      ids <- view_li %>% 
        filter(entity.profileRef.id == x) %>% 
        select(linkId, entity.profileRef.webPropertyId)
      
    # as only one view per loop, the webProperty should be the same for each
    wp <- ids$entity.profileRef.webPropertyId[[1]]
      
    ga_users_delete_linkid(ids$linkId, 
                           accountId = accountId, 
                           webPropertyId = wp, 
                           viewId = x,
                           check = FALSE)
    })
    
  }
  
  myMessage("All references to email deleted")
  TRUE
  
  
}

#' Delete users access from account, webproperty or view level
#' 
#' @param linkId The linkId(s) that is available using \link{ga_users_list} e.g. \code{47480439:104185380183364788718}
#' @inheritParams ga_users_list
#' @param check If the default \code{TRUE} will check that the user has user access at the level you are trying to delete them from - if not will throw an error.
#' 
#' @description 
#' 
#' The \code{linkId} is in the form of the accountId/webPropertyId/viewId colon separated from a link unique Id.
#' 
#' Delete user access by supplying the linkId for that user at the level they have been given access.  It won't work to delete user links at account level if they have been assigned at web property or view level - you will need to get the linkId for that level instead. e.g. a user needs \code{permissions.local} to be non-NULL to be deleted at that level.  The parameter \code{check} will do this check before deletion and throw an error if they can not be deleted.   Set this to \code{check=FALSE} to suppress this behaviour.
#' 
#' If you supply more than one \code{linkId}, then batch processing will be applied.  Batching has special rules that give you 30 operations for the cost of one API call against your quota. When batching you will only get a \code{TRUE} result on successful batch, but individual \code{linkId}s may have failed.  Check via \link{ga_users_list} afterwards and try to delete individual linkIds to get more descriptive error messages. 
#' 
#' @return TRUE if the deletion is successful, an error if not. 
#' @importFrom googleAuthR gar_api_generator gar_batch_walk
#' 
#' @import assertthat
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountUserLinks/delete}{Google Documentation}
#' @family User management functions
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' # get the linkId for the user you want to delete
#' ga_users_list(47480439, webPropertyId = "UA-47480439-2", viewId = 81416156)
#' ga_users_delete_linkid("81416156:114834495587136933146", 
#'                        accountId = 47480439, 
#'                        webPropertyId = "UA-47480439-2", 
#'                        viewId = 81416156)
#' 
#' # check its gone
#' ga_users_list(47480439, webPropertyId = "UA-47480439-2", viewId = 81416156)
#' 
#' # can only delete at level user has access, the above deletion woud have failed if via:
#' ga_users_delete_linkid("47480439:114834495587136933146", 47480439)
#' 
#' }
ga_users_delete_linkid <- function(linkId,
                            accountId,
                            webPropertyId = NULL,
                            viewId = NULL,
                            check = TRUE){
  default_project_message()
  assert_that(is.character(linkId), is.flag(check))
  accountId <- as.character(accountId)
  
  if(check){
    check_me <- ga_users_list(accountId, webPropertyId, viewId)
    if(!any(linkId %in% check_me$linkId)){
      stop(sprintf("linkId %s not found at this level", linkId), call. = FALSE)
    }
    the_check <- check_me[check_me$linkId == linkId, "permissions.local"] != ""
    if(!the_check){
      stop(sprintf("Requested deletion of linkId %s for email %s does not have permissions.local at this level of accountId/webPropertyId/viewId requested.  linkId's can only be deleted from levels they were given access.",
                   linkId, check_me[check_me$linkId == linkId, "userRef.email"]),
                   call. = FALSE)
    }
  }
  
  if(length(linkId) == 1){
    the_url <- sprintf("%s/%s",
                       make_user_url(accountId, webPropertyId, viewId),
                       linkId)
    
    users <- gar_api_generator(the_url, "DELETE")
    
    res <- suppressWarnings(users())
    if(res$status_code == 204){
      myMessage("Successfully deleted linkId: ", linkId, level = 3)
    } else {
      stop("Problem deleting linkId: ", linkId, call. = FALSE)
    }
  } else {
    # batched deletion
    myMessage("Batching delete users - every 30 batched counts as one in quota.", 
              level = 3)
    base_url <- "https://www.googleapis.com/analytics/v3/management"
    path_args <- list(
      accounts = accountId,
      webproperties = webPropertyId,
      profiles = viewId,
      entityUserLinks = linkId[[1]]
    )

    users <- gar_api_generator(base_url, "DELETE", 
                               path_args = path_args,
                               data_parse_function = function(x) x)
    
    batched <- gar_batch_walk(users,
                              walk_vector = linkId, 
                              gar_paths = path_args,
                              path_walk = "entityUserLinks",
                              batch_size = 300,
                              data_frame_output = FALSE)
    myMessage("Batched deletion of users successful", level = 3)

  }
  
  TRUE
}

#' Create or update user access to Google Analytics
#' 
#' @param email The email(s) of the user(s) to add.  Has to have a Google account. 
#' @param permissions Which permissions to add as a vector - \code{"MANAGE_USERS"},\code{"EDIT"},\code{"COLLABORATE"},\code{"READ_AND_ANALYZE"}
#' @inheritParams ga_users_list
#' 
#' @description 
#' 
#' If you supply more than one \code{email}, then batch processing will be applied.  Batching has special rules that give you 30 operations for the cost of one API call against your quota. When batching you will only get a \code{TRUE} result on successful batch, but individual entries may have failed.  Check via \link{ga_users_list} afterwards and try to add individual linkIds to get more descriptive error messages. 
#' 
#' @return \code{TRUE} if successful
#' @family User management functions
#' @import assertthat
#' 
#' @importFrom googleAuthR gar_api_generator gar_batch_walk
#' @export
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/user-management}{Google help article on user permissions}
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' ga_users_add(c("the_email@company.com", "another_email@company.com"), 
#'              permissions = "EDIT", accountId = 47480439)
#' 
#' }
ga_users_add <- function(email, 
                         permissions,
                         accountId,
                         webPropertyId=NULL,
                         viewId=NULL){
  default_project_message()
  accountId <- as.character(accountId)
  
  assert_that(
    is.character(email),
    is.character(permissions),
    is.string(accountId),
    all(permissions %in% c("MANAGE_USERS","EDIT","COLLABORATE","READ_AND_ANALYZE"))
  )
  
  the_url <- make_user_url(accountId, webPropertyId, viewId)
  
  users <- gar_api_generator(the_url, "POST", data_parse_function = function(x) x)
  
  if(length(email) == 1){

    the_body <- list(
      permissions = list(
        local = list(permissions)
      ),
      userRef = list(
        email = email
      )
    )

    res <- tryCatch(users(the_body = the_body),
                    error = function(err){
                      stop("Make sure email has a Google account - ", err, call. = FALSE)
                    })
    
    if(res$kind != "analytics#entityUserLink"){
      stop("Didn't add user email: ", email)
    }
    
    myMessage(sprintf("Successfully added %s to %s with linkId: %s", 
                      email, paste(accountId, webPropertyId, viewId, collapse = " "),  
                      res$id), 
              level = 3)
  } else {
    myMessage("Batching adding users - every 30 batched counts as one in quota.", level = 3)
    
    changed_emails <- lapply(email, function(x){userRef = list(email = x)})
    
    batched <- gar_batch_walk(users, 
                              walk_vector = changed_emails, 
                              the_body = list(
                                permissions = list(
                                  local = list(permissions)
                                ),
                                userRef = list(
                                  email = email[[1]]
                                )
                              ),
                              body_walk = "userRef",
                              batch_size = 300,
                              data_frame_output = FALSE)
    
  }

  TRUE
  
}

#' Update a user access in Google Analytics
#' 
#' This is for altering existing user access.  
#' 
#' @param linkId The linkId to update
#' @inheritParams ga_users_list
#' @param update_object A list that will be turned into JSON via \link[jsonlite]{toJSON} that represents the new configuration for this linkId
#' 
#' 
#' @return The new user object that has been altered.
#' @family User management functions
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator gar_batch_walk
#' @export
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/user-management}{Google help article on user permissions}
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' # the update to perform
#' o <- list(permissions = list(local = list("EDIT")))
#' 
#' ga_users_update("UA-123456-1:1111222233334444",
#'                 update_object = o,
#'                 accountId = 47480439,
#'                 webPropertyId = "UA-123456-1")
#'                 
#' }
ga_users_update <- function(linkId,
                            update_object,
                            accountId,
                            webPropertyId = NULL,
                            viewId = NULL){
  default_project_message()
  accountId <- as.character(accountId)
  
  assert_that(
    is.character(linkId),
    is.list(update_object)
  )
  
  # batched deletion
  base_url <- "https://www.googleapis.com/analytics/v3/management/"
  path_args <- list(
    accounts = accountId,
    webproperties = webPropertyId,
    profiles = viewId,
    entityUserLinks = linkId
  )
  users <- gar_api_generator(base_url, "PUT",
                             path_args = path_args,
                             checkTrailingSlash = FALSE,
                             data_parse_function = function(x) x)
  myMessage("Batching update users - every 30 batched counts as one in quota.", level = 3)
  batched <- gar_batch_walk(users,
                            walk_vector = linkId,
                            gar_paths = path_args,
                            the_body = update_object,
                            path_walk = "entityUserLinks",
                            batch_size = 300,
                            data_frame_output = FALSE)
  myMessage("Batched update of users successful", level = 3)

  TRUE
}


make_user_url <- function(accountId, webPropertyId, viewId){
  
  baseurl <- "https://www.googleapis.com/analytics/v3/management"
  aurl <- sprintf("%s/accounts/%s/entityUserLinks",
                 baseurl, accountId)
  
  if(!is.null(webPropertyId)){
    aurl <- sprintf("%s/accounts/%s/webproperties/%s/entityUserLinks",
                   baseurl, accountId, webPropertyId)
  }
  
  if(!is.null(viewId)){
    aurl <- sprintf("%s/accounts/%s/webproperties/%s/profiles/%s/entityUserLinks",
                   baseurl, accountId, webPropertyId, viewId)
  }
  
  aurl
}

