#' Create or update a user deletion request
#' 
#' The Google Analytics User Deletion API allows customers to process deletions of data associated with a given user identifier. 
#' 
#' @param userId User ID
#' @param idType Type of user. One of \code{APP_INSTANCE_ID, CLIENT_ID or USER_ID}.
#' @param propertyId The Google Analytics Web property or Firebase ProjectId you are deleting the user from.
#' @param propertyType Firebase or Google Analytics
#' 
#' @details 
#' 
#' The user explorer report in Google Analytics can give you the client.id you need to test. 
#' 
#' A data deletion request can be applied to either a Google Analytics web property (specified by \code{propertyType="ga"}) or Firebase application \code{propertyType="firebase"}. A user whose data will be deleted can be specified by setting one of the identifiers the \code{userId} field. The type of the identifier must be specified inside \code{idType} field.
#' 
#' @return The upsert method returns a User Deletion Request Resource with \code{deletionRequestTime} field set. This field is the point in time up to which all user data will be deleted. This means that all user data for the specified user identifier and Google Analytics property or Firebase project will be deleted up to this date and time - if the user with the same identifier returns after this date/time, they will reappear in reporting. 
#' 
#' @seealso \url{https://developers.google.com/analytics/devguides/config/userdeletion/v3/}
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # make sure you are authenticated with user deletion scopes
#' options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics.user.deletion")
#' ga_auth(new_user = TRUE)
#' 
#' # the user
#' ga_user_deletion("12345.1521059208", "UA-12345-2")
#' 
#' #$kind
#' #[1] "analytics#userDeletionRequest"
#' 
#' #$id
#' #$id$type
#' #[1] "CLIENT_ID"
#' #
#' #$id$userId
#' #[1] "12345.1521059208"
#' #$webPropertyId
#' #[1] "UA-12345-2"
#' 
#' #$deletionRequestTime
#' #[1] "2018-05-19T21:23:46.855Z"
#' 
#' }
#' 
#' @export
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
ga_user_deletion <- function(userId,
                             propertyId,
                             idType = c("CLIENT_ID", "USER_ID", "APP_INSTANCE_ID"),
                             propertyType= c("ga", "firebase")){
  
  idType       <- match.arg(idType)
  propertyType <- match.arg(propertyType)
  
  assert_that(
    is.string(userId),
    is.string(propertyId)
  )
  
  body <- list(
    kind = "analytics#userDeletionRequest",
    id = list(
      type = idType,
      userId = userId
    )
  )
  
  if(propertyType == "ga"){
    body <- c(body, list(webPropertyId = propertyId))
  } else if(propertyType == "firebase"){
    body <- c(body, list(firebaseProjectId = propertyId))    
  } 
  
  call_api <- gar_api_generator("https://www.googleapis.com/analytics/v3/userDeletion/userDeletionRequests:upsert",
                                "POST",
                                data_parse_function = function(x) x)
  
  call_api(the_body = body)
  
}
