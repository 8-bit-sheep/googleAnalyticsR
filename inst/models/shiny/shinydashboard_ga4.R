library(shiny)
library(shinydashboard)
library(googleAuthR)
library(googleAnalyticsR)

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")
options(googleAuthR.redirect = "{{ deployed_url }}")

# loads a pre-existing model
model <- ga_model_load("{{ model1 }}")
modelUi <- model$shiny_module$ui
modelServer <- model$shiny_module$server

header <- dashboardHeader(
  title = "{{ shiny_title }}",
  titleWidth = 250
)
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Logout", href = "/"),
    dateRangeInput("dates", label = "Dates", 
                   start = Sys.Date()-366, end = Sys.Date()-1),
    metricDimensionSelectUI("mets1")
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
        modelUi("model1")
        )
  )
)

skin <- "{{ skin }}"
if(!nzchar(skin)) skin <- "blue"

## ui.R
ui <- dashboardPage(header, sidebar, body, skin = skin)

## server.R
server <- function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  accs <- reactive({
    req(token)
    ga_account_list("ga4")
  })
  
  # module for authentication
  property_id <- accountPicker("auth_menu", ga_table = accs, id_only = TRUE)
  
  meta <- reactive({
    req(property_id())
    
    ga_meta("data", propertyId = property_id())
  })
  
  metrics <- metricDimensionSelect(
    "mets1", 
    default = "sessions",
    custom_meta = meta())
  
  output$model_description <- renderText(model$description)

  # module to display model results
  modelServer("model1", 
              view_id = property_id, 
              date_range = reactive(input$dates),
              metrics = metrics)
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
