#' Setup wizard for authentication options
#' 
#' @export
#' @importFrom googleAuthR gar_setup_clientid
ga_auth_setup <- function(){
  
  op <- gar_setup_menu(
    c("Setup your own client ID and secret",
      "Setup auto-authentication (OAuth2 email)",
      "Create and download JSON service account key",
      "Setup auto-authentication (JSON service account key)"), 
    package_name = "googleAnalyticsR"
  )
  
  session_user <- gar_setup_check_session()
  
  gar_setup_menu_do(op,
                    trigger = 1,
                    do_function = gar_setup_clientid,
                    session_user = session_user)
  
  gar_setup_menu_do(op,
                    trigger = 2,
                    do_function = gar_setup_env_check,
                    env_arg = "GARGLE_EMAIL",
                    edit_option = TRUE,
                    set_to = setup_gargle_email(),
                    session_user = session_user)
  
  gar_setup_menu_do(op,
                    trigger = 3,
                    do_function = gar_setup_auth_key,
                    session_user = session_user,
                    email = "",
                    file = "googleanalyticsr-auth-key.json",
                    default_key = "googleanalyticsr")
  
  gar_setup_menu_do(op,
                    trigger = 4,
                    do_function = gar_setup_env_check,
                    env_arg = "GA_AUTH_FILE",
                    set_to = gar_setup_get_authenv(session_user = session_user,
                                                   env_arg = "GA_AUTH_FILE"),
                    edit_option = TRUE,
                    stop = TRUE,
                    session_user = session_user)
  
  return(invisible(""))
}

#returns gargle string for env
setup_gargle_email <- function(){
  ee <- readline("Email to be used for auto-authentication: ")
  paste0("GARGLE_EMAIL=",ee)
}