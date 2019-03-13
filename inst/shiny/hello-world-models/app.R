library(shiny)
library(googleAuthR)
library(googleAnalyticsR) 

gar_set_client(web_json = "ga-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")

# loads a pre-existing model
model <- ga_model_example("decomp_ga.gamr", location = "googleAnalyticsR")

## ui.R
ui <- fluidPage(title = "googleAnalyticsR Test Deployment",
                
                authDropdownUI("auth_menu"),
                h2("Model Description"),
                textOutput("model_description"),
                h2("Model Output"),
                model$shiny_module$ui("model1")
                
)

## server.R
server <- function(input, output, session){
  
  gar_shiny_auth(session)
  
  al <- reactive(ga_account_list())
  view_id <- callModule(authDropdown, "auth_menu", ga.table = al)
  output$model_description <- renderText(model$description)
  callModule(model$shiny_module$server, "model1", view_id = view_id)
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
