
# ---start server_app_boilerplate.R
server <- function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  {{{ auth_accounts }}}
  
  # module for authentication
  view_id <- {{ auth_server }}
  
  # module to display model results
  {{{ model_server }}}
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
# ---end server_app_boilerplate.R
