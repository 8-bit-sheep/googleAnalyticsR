library(shiny)
library(googleAuthR)
library(googleAnalyticsR)

options(shiny.port = 1221)
source("modules.R")

shinyServer(function(input, output, session){
  
  token <- callModule(googleAuth, "login")
  
  ga_accounts <- reactive({
    validate(
      need(token(), "Authenticate")
    )
    
    with_shiny(google_analytics_account_list, shiny_access_token = token())
  })
  
  selected_id <- callModule(authDropdown, "auth_menu", ga.table = ga_accounts)
  
  segment_built <- callModule(segmentBuilder, "demo_segments")
  
})