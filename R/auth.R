#' Authenticate with Google Analytics OAuth2
#'
#' A wrapper for \link[googleAuthR]{gar_auth} and \link[googleAuthR]{gar_auth_service}
#'
#' @param token An existing token or file location of a token to authenticate with
#' @param email An existing cached email to authenticate with or \code{TRUE} to authenticate with only email available.  If not set then you will get an interactive prompt asking you to choose which email to authenticate with.
#' @param json_file Authentication service key you have downloaded from your Google Project - an alternative to OAuth2 email authentication
#' 
#' @details
#' 
#' Run this function first time to authenticate with Google in your browser.  
#' 
#' After initial authentication, your authentication details will be kept globally for use later, tied to your email, and the next time you authenticate you will be given a prompt to choose which email to authentcate from.  Set \code{email="your@email.com"} to skip the interactive prompt.
#' 
#' @section Multiple accounts:
#' 
#' You can authenticate with a new email for each account. Supply a different email to use those details for your session.
#'
#' @section Service accounts:
#'   
#' If you use the service account JSON, you will need to add the service account email 
#'   to your Google Analytics users to see data e.g. \code{xxxx@yyyyyy.iam.gserviceaccount.com}
#'   
#' @section Auto-authentication:
#' 
#' You can choose to auto-authenticate by creating a Google OAuth service account JSON file.
#' 
#' Specify an environment variable in R via a \code{.Renviron} file or using \link{Sys.setenv} which points to the file location of your chosen authentication file.  See \link{Startup}
#' 
#' Once you have set the environment variable \code{GA_AUTH_FILE} to a valid file location,
#'   the function will look there for authentication details upon loading the library meaning 
#'   you will not need to call \code{ga_auth()} yourself as you would normally.
#' 
#' An example \code{.Renviron} file is below:
#' 
#' \code{GA_AUTH_FILE = "/Users/bob/auth/googleAnalyticsR.json"}
#'
#' \code{GA_AUTH_FILE} can be a service account JSON ending with file extension \code{.json}.  Make sure to give the service account email access to your Google Analytics account as mentioned above. 
#'
#' @section Your own Google Project:
#' 
#' Be default the Google Project used is shared by all users, so you may find it runs out of API calls.  
#' To mitigate that, create your own Google Project and turn on the Analytics APIs.
#' 
#' The best way to do this is to use \link[googleAuthR]{gar_set_client} by downloading your JSON client credentials and setting them to be found on package startup via the \code{GAR_CLIENT_JSON} environment argument.  See \code{?googleAuthR::gar_set_client} function help pages for details.
#' 
#' Or you can then copy your Google Cloud Project's client ID and client secret, to place in options or environment arguments (whichever is easiest)
#' 
#' The environment args are below.  Similar to auto-authentication, you can place your entries in an \code{.Renviron} file
#' 
#' \code{
#' GA_CLIENT_ID="XXXX"
#' GA_CLIENT_SECRET="XXX"
#' GA_WEB_CLIENT_ID="XXX"
#' GA_WEB_CLIENT_SECRET="XXX"
#' }
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # to use default package credentials (for testing)
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' # to use your own Google Cloud Project credentials
#' # go to GCP console and download client credentials JSON 
#' # ideally set this in .Renviron file, not here but just for demonstration
#' Sys.setenv("GAR_CLIENT_JSON" = "location/of/file.json")
#' library(googleAnalyticsR)
#' # should now be able to log in via your own GCP project
#' ga_auth()
#' 
#' # reauthentication
#' # Once you have authenticated, set email to skip the interactive message
#' ga_auth(email = "my@email.com")
#' 
#' # or leave unset to bring up menu on which email to auth with
#' ga_auth()
#' # The googleAnalyticsR package is requesting access to your Google account. 
#' # Select a pre-authorised account or enter '0' to obtain a new token.
#' # Press Esc/Ctrl + C to abort.
#' #1: my@email.com
#' #2: work@mybusiness.com

#' # you can set authentication for many emails, then switch between them e.g.
#' ga_auth(email = "my@email.com")
#' ga_account_list() # lists one set of accounts
#' ga_auth(email = "work@mybusiness.com") 
#' ga_account_list() # lists second set of accounts
#' 
#' # or authenticate via the service key, that has been added to the GA as a user
#' ga_auth(json_file = "service-key.json")
#' 
#' }
#'
#' @return Invisibly, the token that has been saved to the session
#' @importFrom googleAuthR gar_auth_service gar_auth
#' @importFrom tools file_ext
#' @export
ga_auth <- function(token = NULL, email = NULL, json_file = NULL){
  
  if(!is.null(json_file)){
    client_email <- jsonlite::read_json(json_file)$client_email
    cli::cli_alert_info(paste("Authenticating using", client_email))
    return(gar_auth_service(json_file))
  }

  # client.id only needed for OAuth2 auth
  default_project_message()
  
  if(nzchar(Sys.getenv("GARGLE_EMAIL"))){
    email <- Sys.getenv("GARGLE_EMAIL")
    myMessage("Using email from GARGLE_EMAIL env var:", email, level = 3)
  }
  
  gar_auth(token = token,
           email = email,
           package = "googleAnalyticsR")
 
}
