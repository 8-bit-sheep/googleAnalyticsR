library(shiny)
library(googleAnalyticsR) # for accountPicker
library(googleAuthR)      # for gar_shiny_ui

ui <- fluidPage(title = "Shiny App",
                accountPickerUI("auth_menu", inColumns = TRUE))
server <- function(input, output, session){
  token <- gar_shiny_auth(session)
  
  accs <- reactive({
    req(token)
    ga_account_list("ga4")
  })
  
  # module for authentication
  property_id <- accountPicker("auth_menu", ga_table = accs, id_only = TRUE)
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)