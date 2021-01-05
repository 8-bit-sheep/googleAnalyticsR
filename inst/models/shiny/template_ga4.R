library(shiny)
library(googleAuthR)
library(googleAnalyticsR)

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

# loads a pre-existing model
model <- ga_model_load("{{ ga_model }}")

modelUi <- model$shiny_module$ui
modelServer <- model$shiny_module$server

## ui.R
ui <- fluidPage(title = "{{ shiny_title }}",
                accountPickerUI("auth_menu", inColumns = TRUE),
                h2("Model Description"),
                textOutput("model_description"),
                h2("Model Output"),
                modelUi("{{ ga_model_name }}")
                
)

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
