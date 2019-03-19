library(shiny)
library(googleAuthR)
library(googleAnalyticsR) 

gar_set_client(web_json = "ga-web-client.json",
               scopes = "https://www.googleapis.com/auth/analytics.readonly")

options(googleAuthR.redirect = "https://mark.shinyapps.io/googleAnalyticsR_test_deployment/")

## ui.R
ui <- fluidPage(title = "googleAnalyticsR Test Deployment",
                
      authDropdownUI("auth_menu"),
      textOutput("viewid"),
      textOutput("client_id")
      
)

## server.R
server <- function(input, output, session){

  gar_shiny_auth(session)
  
  al <- reactive(ga_account_list())
  view_id <- callModule(authDropdown, "auth_menu", ga.table = al)
  
  output$viewid <- renderText(view_id())
  
  output$client_id <- renderText(getOption("googleAuthR.webapp.client_id"))

}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
