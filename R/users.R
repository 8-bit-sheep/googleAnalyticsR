#' List Users
#' 
#' Will list users on an account, webproperty or view level
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id - leave NULL to operate on account level only
#' @param viewId viewId - leave NULL to operate on webProperty level only
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
#' }
ga_users_list <- function(accountId,
                          webPropertyId = NULL,
                          viewId = NULL){
  accountId <- as.character(accountId)
  
  users <- gar_api_generator(make_user_url(accountId, webPropertyId, viewId),
                             "GET",
                             data_parse_function = parse_ga_users_list)
  
  pages <- gar_api_page(users, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' Delete users access from account, webproperty or view level
#' 
#' @param linkId The linkId that is available using \link{ga_users_list} e.g. \code{47480439:104185380183364788718}
#' @inheritParams ga_users_list
#' @param check If the default \code{TRUE} will check that the user has user access at the level you are trying to delete them from - if not will throw an error.
#' 
#' @description 
#' 
#' The \code{linkId} is in the form of the accountId/webPropertyId/viewId colon seperated from a link unique Id.
#' 
#' Delete user access by supplying the linkId for that user at the level they have been given access.  It won't work to delete user links at account level if they have been assigned at web property or view level - you will need to get the linkId for that level instead. e.g. a user needs \code{permissions.local} to be non-NULL to be deleted at that level.  The parameter \code{check} will do this check before deletion and throw an error if they can not be deleted.   Set this to \code{check=FALSE} to suppress this behaviour.
#' 
#' @return TRUE if the deletion is successful, an error if not. 
#' @importFrom googleAuthR gar_api_generator
#' 
#' @import assertthat
#' @seealso \href{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountUserLinks/delete}{Google Docuemntation}
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
#' ga_users_delete("81416156:114834495587136933146", 47480439, webPropertyId = "UA-47480439-2", viewId = 81416156)
#' 
#' # check its gone
#' ga_users_list(47480439, webPropertyId = "UA-47480439-2", viewId = 81416156)
#' 
#' # can only delete at level user has access, the above deletion woud have failed if via:
#' ga_users_delete("47480439:114834495587136933146", 47480439)
#' 
#' }
ga_users_delete <- function(linkId,
                            accountId,
                            webPropertyId = NULL,
                            viewId = NULL,
                            check = TRUE){
  assert_that(is.string(linkId), is.flag(check))
  accountId <- as.character(accountId)
  
  if(check){
    check_me <- ga_users_list(accountId, webPropertyId, viewId)
    if(!linkId %in% check_me$linkId){
      stop(sprintf("linkId %s not found at this level", linkId), call. = FALSE)
    }
    the_check <- check_me[check_me$linkId == linkId, "permissions.local"] != ""
    if(!the_check){
      stop(sprintf("Requested deletion of linkId %s for email %s does not have permissions.local at this level of accountId/webPropertyId/viewId requested.  linkId's can only be deleted from levels they were given access.",
                   linkId, check_me[check_me$linkId == linkId, "userRef.email"]),
                   call. = FALSE)
    }
  }

  the_url <- sprintf("%s/%s",
                     make_user_url(accountId, webPropertyId, viewId),
                     linkId)
  
  users <- gar_api_generator(the_url,
                             "DELETE")
  res <- suppressWarnings(users())
  if(res$status_code == 204){
    myMessage("Successfully deleted linkId: ", linkId, level = 3)
    return(TRUE)
  } else {
    stop("Problem deleting linkId: ", linkId, call. = FALSE)
  }
  
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

#' @noRd
#' @import assertthat
#' @importFrom dplyr rename select ends_with
parse_ga_users_list <- function(x){
  
  x %>% 
    management_api_parsing("analytics#entityUserLinks") %>% 
    select(-userRef.kind, -ends_with("kind"), -ends_with("href")) %>% 
    rename(linkId = id)
  
}