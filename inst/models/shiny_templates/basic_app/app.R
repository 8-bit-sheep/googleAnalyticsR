## basic_full ui.R
library(shiny)
library(googleAuthR)
library(googleAnalyticsR)
{{{ model_libraries }}}

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}", 
               activate = "web")
options(googleAuthR.redirect = "{{ deployed_url }}")

# loads pre-existing models
{{{ model_load }}}

## ui.R
ui <- fluidPage(title = "{{ shiny_title }}",
                {{ auth_ui }},
                {{{ date_range }}},
                h2("Model Output"),
{{{ model_ui }}}
                
)

server <- function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  {{{ auth_accounts }}}
  
  # module for authentication
  view_id <- {{ auth_server }}
  
  # module to display model results
  {{{ model_server }}}
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
