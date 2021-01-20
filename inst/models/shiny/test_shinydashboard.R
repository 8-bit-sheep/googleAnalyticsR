library(shiny)
library(shinydashboard)
library(googleAuthR)
library(googleAnalyticsR)
library(CausalImpact)
library(xts)
library(tidyr)
library(googleAnalyticsR)
library(assertthat)
library(dygraphs)

gar_set_client(web_json = "/Users/mark/dev/auth/mark-edmondson-gde-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")
options(googleAuthR.redirect = "")

# loads a pre-existing model
model1 <- ga_model_shiny_load('/private/var/folders/94/4rb8rsbd3bqbh0y67cvpq4q00000gn/T/RtmpoL3C5u/file4b25744f4723.gamr')

header <- dashboardHeader(
  title = "ga_model_shiny",
  titleWidth = 250
)
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Logout", href = "/"),
    # custom macro for shiny dashboard
    menuItem('Causal Impact ', tabName = 'model1')
  )
)
body <- dashboardBody(
  fluidRow(
    box(width=12,
        authDropdownUI('auth_menu', inColumns = TRUE),
        dateRangeInput("date_range", 
                        "Date Range", 
                        start = Sys.Date() - 400,
                        end = Sys.Date() - 1)
        )
    ),
  tabItems(
    tabItem(tabName = 'model1',
         model1$ui('model1'))
  )
)

skin <- ""
if(!nzchar(skin)) skin <- "blue"

## ui.R
ui <- dashboardPage(header, sidebar, body, skin = skin)

## server.R
server <- function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  al <- reactive({req(token);ga_account_list()})
  
  # module for authentication
  view_id <- callModule(authDropdown, 'auth_menu', ga.table = al)
  
  # module to display model results
  model1$server('model1', view_id = view_id, 
                         date_range = reactive(input$date_range))
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
