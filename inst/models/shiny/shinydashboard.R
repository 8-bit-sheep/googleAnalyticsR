library(shiny)
library(shinydashboard)
library(googleAuthR)
library(googleAnalyticsR)
{{{ model_libraries }}}

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")
options(googleAuthR.redirect = "{{ deployed_url }}")

# loads a pre-existing model
{{{ model_load }}}

header <- dashboardHeader(
  title = "{{ shiny_title }}",
  titleWidth = 250
)
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Logout", href = "/"),
    # custom macro for shiny dashboard
    {{{ model_tabs }}}
  )
)
body <- dashboardBody(
  fluidRow(
    box(width=12,
        {{ auth_ui }},
        {{{ date_range }}}
        )
    ),
  tabItems(
    {{{ model_ui }}}
  )
)

skin <- "{{ skin }}"
if(!nzchar(skin)) skin <- "blue"

## ui.R
ui <- dashboardPage(header, sidebar, body, skin = skin)

## server.R
server <- function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  {{{ auth_accounts }}}
  
  # module for authentication
  view_id <- {{ auth_server }}
  
  # module to display model results
  {{{ model_server }}}
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
