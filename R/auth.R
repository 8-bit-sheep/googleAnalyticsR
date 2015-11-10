#' ga_auth
#'
#' @param token If you have an existing token already
#' @param new_user Go through the auth flow again
#' @param verbose More feedback
#'
#' @export
ga_auth <- function(token = NULL,
                    new_user = FALSE,
                    verbose = TRUE){
  options("googleAuthR.client_id" = "858905045851-3beqpmsufml9d7v5d1pr74m9lnbueak2.apps.googleusercontent.com")
  options("googleAuthR.client_secret" = "bnmF6C-ScpSR68knbGrHBQrS")
  options("googleAuthR.jsonlite.simplifyVector" = FALSE)
  options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics", "https://www.googleapis.com/auth/analytics.readonly"))
  options("googleAnalyticsR.raw_req" = FALSE)
  
  googleAuthR::gar_auth(token = token,
                        new_user = new_user,
                        verbose = verbose)
  
}