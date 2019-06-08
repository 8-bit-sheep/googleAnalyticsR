library(shiny)
library(googleAuthR)
library(googleAnalyticsR) 
library(dygraphs)
library(xts)
library(tidyr)

gar_set_client(web_json = "ga-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")

options(googleAuthR.redirect = "http://localhost:1221")

# loads a pre-existing model
model <- ga_model_example("ga-effect.gamr", location = "googleAnalyticsR")

## ui.R
ui <- fluidPage(title = "googleAnalyticsR GA-Effect Model",
                
      authDropdownUI("auth_menu"),
      h2("Model Description"),
      textOutput("model_description"),
      h2("Model Output"),
      dateInput("event_date", 
                label = "Event Date", 
                value = Sys.Date() - 100,
                max = Sys.Date(),
                min = Sys.Date() - 600),
      model$shiny_module$ui("model1")
      
)

## server.R
server <- function(input, output, session){

  gar_shiny_auth(session)
  
  al <- reactive(ga_account_list())
  view_id <- callModule(authDropdown, "auth_menu", ga.table = al)
  output$model_description <- renderText(model$description)
  callModule(model$shiny_module$server, "model1", 
             view_id = view_id, 
             event_date = reactive(input$event_date))

}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
