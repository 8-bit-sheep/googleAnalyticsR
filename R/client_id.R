#' Get hashed version of client id (also known as hashClientId, hashedClientId, or BigQuery's fullVisitorId)
#' 
#' @param webPropertyId Web Property Id
#' @param clientId Client Id
#'
#' @return hashedClientId object list
#' @importFrom googleAuthR gar_api_generator
#' @family clientid functions
#' @export
ga_clientid_hash <- function(webPropertyId, clientId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/clientId:hashClientId"
  
  body <- list(
    webPropertyId = webPropertyId,
    clientId = clientId
  )
  
  hashed <- gar_api_generator(url,
    "POST",
    data_parse_function = function(x) x
  )
  
  hashed(the_body = body)
}

#' Create or update a user deletion request
#' 
#' The Google Analytics User Deletion API allows customers to process deletions of data associated with a given user identifier. 
#' 
#' @param userId A character vector of user ID's
#' @param idType Type of user. One of `APP_INSTANCE_ID, CLIENT_ID or USER_ID`.
#' @param propertyId The Google Analytics Web property or Firebase ProjectId you are deleting the user from.
#' @param propertyType Firebase or Google Analytics
#' 
#' @details 
#' 
#' The user explorer report in Google Analytics can give you the client.id you need to test. 
#' 
#' A data deletion request can be applied to either a Google Analytics web property (specified by `propertyType="ga"`) or Firebase application (`propertyType="firebase"`). A user whose data will be deleted can be specified by setting one of the identifiers the `userId` field. The type of the identifier must be specified inside `idType` field.
#' 
#' There is a quota of 500 queries per day per cloud project.
#' 
#' The API returns a User Deletion Request Resource with `deletionRequestTime` field set. This field is the point in time up to which all user data will be deleted. This means that all user data for the specified user identifier and Google Analytics property or Firebase project will be deleted up to this date and time - if the user with the same identifier returns after this date/time, they will reappear in reporting. 
#' 
#' @return a data.frame with a row for each userID you sent in, plus a column with its `deletionRequestTime`
#' 
#' @seealso <https://developers.google.com/analytics/devguides/config/userdeletion/v3/>
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # make sure you are authenticated with user deletion scopes
#' options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/analytics.user.deletion")
#' ga_auth()
#' 
#' # a vector of ids
#' ids <- c("1489547420.1526330722", "1138076389.1526568883")
#' 
#' # do the deletions
#' ga_clientid_deletion(ids, "UA-1234-2")
#' #                 userId   id_type  property      deletionRequestTime
#' #1 1489547420.1526330722 CLIENT_ID UA-1234-2 2018-05-20T19:43:33.540Z
#' #2 1138076389.1526568883 CLIENT_ID UA-1234-2 2018-05-20T19:43:36.218Z
#' 
#' }
#' 
#' @export
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @family clientid functions
ga_clientid_deletion <- function(userId,
                             propertyId,
                             idType = c("CLIENT_ID", "USER_ID", "APP_INSTANCE_ID"),
                             propertyType= c("ga", "firebase", "ga4")){
  idType       <- match.arg(idType)
  propertyType <- match.arg(propertyType)
  
  assert_that(
    is.character(userId),
    is.string(propertyId)
  )
  
  # a list of one row data.frames
  deletions <- lapply(userId, 
                      ga_user_deletion_one, 
                      propertyId = propertyId,
                      idType = idType,
                      propertyType = propertyType)
  
  # bind to one data.frame
  Reduce(rbind, deletions)
  
}

ga_user_deletion_one <- function(userId,
                                 propertyId,
                                 idType = c("CLIENT_ID", "USER_ID", "APP_INSTANCE_ID"),
                                 propertyType= c("ga", "firebase", "ga4")){
  
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
  } else if( propertyType == "ga4"){
    body <- c(body, list(propertyId = propertyId))
  }
  
  the_url <- "https://www.googleapis.com/analytics/v3/userDeletion/userDeletionRequests:upsert"
  
  call_api <- gar_api_generator(the_url,
                                "POST",
                                data_parse_function = parse_user_deletion)
  
  # rate limit to 1.5 queries per second, or 1 query per 0.66 seconds
  Sys.sleep(0.70)
  
  call_api(the_body = body)
  
}

parse_user_deletion <- function(x){
  
  if(is.null(x$kind)){
    warning("No response object analytics#userDeletionRequest")
    return(NULL)
  }
  
  if(x$kind != "analytics#userDeletionRequest"){
    warning("Response object not analytics#userDeletionRequest")
    return(NULL)
  }
  
  if(!is.null(x$webPropertyId)){
    # ga
    property <- x$webPropertyId
  } else if(!is.null(x$firebaseProjectId)) {
    # firebase
    property <- x$firebaseProjectId
  } else {
    # ga4
    property <- x$propertyId
  }
  
  data.frame(
    stringsAsFactors = FALSE,
    userId  = x$id$userId,
    id_type = x$id$type,
    property = property,
    deletionRequestTime = x$deletionRequestTime
  )
}
