library(shiny)
library(googleAuthR)

gar_set_client(web_json = "ga-web-client.json",
               scopes = c("https://www.googleapis.com/auth/analytics.readonly"))

library(googleAnalyticsR) 
library(tidyverse)

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

shinyApp(gar_shiny_ui(ui, login_ui = gar_shiny_login_ui), server)
