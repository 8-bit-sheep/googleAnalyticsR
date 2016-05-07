#' Authentication with Google Analytics
#' 
#' Wraps \link[googleAuthR]{gar_auth} as a convenience.
#' 
#' Use \code{googleAuthR:gar_auth()} if you need to authenticate with more 
#'   than just the Google Analytics API.
#'   
#' Sets default googleAuthR options, that you will probably want to change via \code{options(googleAuthR.**)}.  
#' 
#' See \link[googleAuthR]{gar_auth} setup details for more details. 
#'
#' @param token If you have an existing token already pass it through.
#' @param new_user Set to TRUE to go through the auth flow again
#'
#' @export
ga_auth <- function(new_user = FALSE,
                    token = NULL){
  options("googleAuthR.client_id" = "289759286325-da3fr5kq4nl4nkhmhs2uft776kdsggbo.apps.googleusercontent.com")
  options("googleAuthR.client_secret" = "1mKySbffYRyWevGkjL0LMJYu")
  options("googleAuthR.jsonlite.simplifyVector" = TRUE)
  options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics", "https://www.googleapis.com/auth/analytics.readonly"))
  options("googleAnalyticsR.webapp.client_id" = "289759286325-42j8nmkeq5n9v9eb1kiuj2i97v9oea1f.apps.googleusercontent.com")
  options("googleAnalyticsR.webapp.client_secret" = "0zBtmZ_klTEzXUaTUTP5AkNQ")
  
  googleAuthR::gar_auth(token = token,
                        new_user = new_user)
  
}