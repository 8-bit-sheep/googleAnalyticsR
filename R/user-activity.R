#' User Activity Request
#' 
#' @param viewId The viewId
#' @param id The user or clientId
#' @param id_type Whether its userId or clientId
#' @param date_range A vector of start and end dates.  If not used will default to a week.
#' 
#' 
#' @export
#' @importFrom googleAuthR gar_api_generator gar_api_page
#' @import assertthat
#' @examples 
#' 
#' \dontrun{
#' 
#' googleAuthR::gar_set_client(scopes = "https://www.googleapis.com/auth/analytics")
#' ga_auth("test.oauth")
#' ga_clientid_activity("1106980347.1461227730", viewId = 81416156)
#' 
#' }
#' @seealso https://developers.google.com/analytics/trusted-testing/user-reporting/
ga_clientid_activity <- function(id, 
                                 viewId, 
                                 id_type = c("CLIENT_ID","USER_ID"), 
                                 date_range = NULL){
  viewId <- as.character(viewId)
  id <- as.character(id)
  id_type <- match.arg(id_type)
  
  if(!is.null(date_range)){
    date_range <- as.character(date_range)
    assert_that(length(date_range) == 2)
    the_dates <- list(
      startDate = date_range[[1]],
      endDate = date_range[[2]]
    )
  } else {
    the_dates <- NULL
  }
  
  body <- list(
    viewId = viewId,
    user = list(
      type = id_type,
      userId = id
    ),
    dateRange = the_dates
  )
  
  body <- rmNullObs(body)
  
  u <- gar_api_generator("https://analyticsreporting.googleapis.com/v4/userActivity:search",
                         "POST",
                         data_parse_function = function(x) x)
  
  u(the_body = body)
  
}

page_user_activity <- function(x){
  attr(x, "nextPageToken")
}

parse_user_activity <- function(x){
  
  o <- x
  attr(o, "nextPageToken") <- x$nextPageToken
  
  o
  
}

