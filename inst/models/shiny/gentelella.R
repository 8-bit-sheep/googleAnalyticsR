library(shiny)             # R webapps
library(gentelellaShiny)   # ui theme
library(googleAuthR)       # auth login
library(googleAnalyticsR) # get google analytics
{{{ model_libraries }}}

# takes JSON client secrets from GAR_CLIENT_WEB_JSON
# set before calls to googleAnalyticsR to make sure it doesn't use default project.
gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

options(googleAuthR.redirect = "{{ deployed_url }}")

# loads a pre-existing models, or NULL if they aren't present
{{{ model_load }}}

ui <- gentelellaPage(
  menuItems = sideBarElement(a("Start Again", href="/")),
  title_tag = "GA time normalised pages",
  site_title = a(class="site_title", icon("clock"), span("Time Normalised")),
  footer = "Made with googleAnalyticsR::ga_model_shiny()",

  # shiny UI elements
  h3("Choose GA account"),
  {{ auth_ui }},
  h3("Time Normalised pages"),
  {{{ model_ui }}},
  br()

)

server <- function(input, output, session) {

  token <- gar_shiny_auth(session)

  {{{ auth_accounts }}}

  # module for authentication
  view_id <- {{ auth_server }}

  # module to display model results
  {{{ model_server }}}

}
# Run the application
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)

