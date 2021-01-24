# ---start server_boilerplate.R
function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  {{{ auth_accounts }}}
  
  # module for authentication
  view_id <- {{ auth_server }}
  
  # module to display model results
  {{{ model_server }}}
  
}
# ---end server_boilerplate.R
