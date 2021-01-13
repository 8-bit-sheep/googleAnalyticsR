library(shiny)             # R webapps
library(gentelellaShiny)   # ui theme
library(googleAuthR)       # auth login
library(googleAnalyticsR) # get google analytics
library(dplyr)
library(plotly)
library(scales)
library(ggplot2)
library(purrr)

# takes JSON client secrets from GAR_CLIENT_WEB_JSON
# set before calls to googleAnalyticsR to make sure it doesn't use default project.
gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

options(googleAuthR.redirect = "{{ deployed_url }}")

model <- ga_model_load(filename = "{{ model1 }}")
modelUi <- model$shiny_module$ui
modelServer <- model$shiny_module$server

ui <- gentelellaPage(
  menuItems = sideBarElement(a("Start Again", href="/")),
  title_tag = "GA time normalised pages",
  site_title = a(class="site_title", icon("clock"), span("Time Normalised")),
  footer = "Made with googleAnalyticsR::ga_model_shiny()",

  # shiny UI elements
  h3("Time Normalised pages"),
  modelUi("model1"),
  br()

)

server <- function(input, output, session) {

  token <- gar_shiny_auth(session)

  al <- reactive({
    req(token)
    ga_account_list()
  })

  # module for authentication
  view_id <- callModule(authDropdown, "auth_dropdown", ga.table = al)

  modelServer("model1", view_id = view_id)

}
# Run the application
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)

