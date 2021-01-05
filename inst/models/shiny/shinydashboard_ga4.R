library(shiny)
library(shinydashboard)
library(googleAuthR)
library(googleAnalyticsR)

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

# loads a pre-existing model
model <- ga_model_load("{{ ga_model }}")

modelUi <- model$shiny_module$ui
modelServer <- model$shiny_module$server

header <- dashboardHeader(
  title = "{{ shiny_title }}"
)
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard")
  )
)
body <- dashboardBody(
  fluidRow(
    box(width=12,
        accountPickerUI("auth_menu", inColumns = TRUE)
        )
    ),
  fluidRow(
    box(width=12,
        modelUi("{{ ga_model_name }}")
        )
  )
)

## ui.R
ui <- dashboardPage(header, sidebar, body, skin = "{{ skin }}")

## server.R
server <- function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  accs <- reactive({
    req(token)
    ga_account_list("ga4")
  })
  
  # module for authentication
  property_id <- accountPicker("auth_menu", ga_table = accs, id_only = TRUE)
  output$model_description <- renderText(model$description)

  # module to display model results
  modelServer("{{ ga_model_name }}", view_id = property_id)
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
