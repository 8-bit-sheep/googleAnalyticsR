#' Setup wizard for authentication options
#' 
#' @export
#' @import googleAuthR
ga_auth_setup <- function(){
  
  op <- gar_setup_menu(
    c("Setup your own client ID and secret",
      "Setup auto-authentication (OAuth2 email)",
      "Create and download JSON service account key",
      "Setup auto-authentication (JSON service account key)",
      "Add a service account email or another email to your GA account"), 
    package_name = "googleAnalyticsR"
  )
  
  if(op != 5){
    session_user <- gar_setup_check_session()
  }
  
  
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
                    session_user = session_user)
  
  gar_setup_menu_do(op,
                    trigger = 5,
                    do_function = add_service_email)
  
  return(invisible(""))
}

#' @noRd
#' @importFrom utils menu
add_service_email <- function(){
  
  which_email <- menu(title = "Which email do you want to add permissions for?",
                      choices = c("The service email from GA_AUTH_FILE",
                                  "Another email"))
  if(which_email == 0){
    return(FALSE)
  }
  
  if(which_email == 1){
    the_email <- jsonlite::read_json(Sys.getenv("GA_AUTH_FILE"))$client_email
  }
  
  if(which_email == 2){
    the_email <- readline("Enter email: ")
  }
  
  if(the_email == ""){
     cli::cli_alert_danger("No email found to configure")
    return(FALSE)
  } 
  
  cli::cli_alert_info("Please now authenticate with an email that has 'manage user' rights for the GA account you need to give {the_email} access to.")
  
  # auth again to use local email
  ga_auth()
  
  access_choices <- c("MANAGE_USERS",
                      "EDIT",
                      "COLLABORATE",
                      "READ_AND_ANALYZE")
  access_level <- menu(title = sprintf("What permission level should %s have?", the_email),
                       choices = access_choices)
  
  access <- access_choices[[access_level]]
  
  cli::cli_alert_info("You have access to these Google Analytics accounts:")
  al <- ga_account_list()
  print(al[, c("accountId", "accountName", "webPropertyId", "viewName", "viewId")])
  cli::cli_rule()
  access_level <- menu(title = "At what level should the permission be set?",
                       choices = c("Account", "Web Property", "View"))
  
  if(access_level == 0){
    cli::cli_alert_danger("No permissions set")
    return(FALSE)
  }
  
  if(access_level == 1){
    account_choices <- unique(al$accountId)
    account_choice <- menu(title = "Which accountId?", choices = account_choices)
    account_id <- account_choices[[account_choice]]
    ga_users_add(the_email, permissions = access, accountId = account_id)
  }
  
  if(access_level == 2){
    wp_choices <- unique(al$webPropertyId)
    wp_choice <- menu(title = "Which webPropertyId?", 
                      choices = wp_choices)
    wp_id <- wp_choices[[wp_choice]]
    account_id <- unique(al[al$webPropertyId == wp_id, "accountId"])
    ga_users_add(the_email, permissions = access, 
                 accountId = account_id,
                 webPropertyId = wp_id)
  }
  
  if(access_level == 3){
    v_choices <- unique(al$viewId)
    v_choice <- menu(title = "Which viewId?", 
                      choices = v_choices)
    v_id <- v_choices[[v_choice]]
    account_id <- unique(al[al$viewId == v_id, "accountId"])
    wp_id <- unique(al[al$viewId == v_id, "webPropertyId"])
    ga_users_add(the_email, permissions = access, 
                 accountId = account_id,
                 webPropertyId = wp_id,
                 viewId = v_id)
  }
  
}

#returns gargle string for env
setup_gargle_email <- function(){
  ee <- readline("Email to be used for auto-authentication: ")
  cli::cli_alert_success("To use this email for GA reporting add this email to the GA View with sufficient rights: ", ee)
  paste0("GARGLE_EMAIL=",ee)
}