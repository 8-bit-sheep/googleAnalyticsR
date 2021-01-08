library(shiny)
library(googleAuthR)
library(googleAnalyticsR)

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

options(googleAuthR.redirect = "{{ deployed_url }}")

# loads a pre-existing model
model <- ga_model_load("{{ ga_model }}")

modelUi <- model$shiny_module$ui
modelServer <- model$shiny_module$server

## ui.R
ui <- fluidPage(title = "{{ shiny_title }}",
                authDropdownUI("auth_menu"),
                h2("Model Description"),
                textOutput("model_description"),
                h2("Model Output"),
                modelUi("{{ ga_model_name }}")
                
)

## server.R
server <- function(input, output, session){
  
  token <- gar_shiny_auth(session)
  
  al <- reactive({
    req(token)
    ga_account_list()
  })
  
  # module for authentication
  view_id <- callModule(authDropdown, "auth_menu", ga.table = al)
  output$model_description <- renderText(model$description)
  
  # module to display model results
  modelServer("{{ ga_model_name }}", view_id = view_id)
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
