library(shiny)
library(googleAuthR)
library(googleAnalyticsR)

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

options(googleAuthR.redirect = "{{ deployed_url }}")

# loads a pre-existing models, or NULL if they aren't present
model1 <- ga_model_shiny_load("{{ model1 }}")
model2 <- ga_model_shiny_load("{{ model2 }}")
model3 <- ga_model_shiny_load("{{ model3 }}")
model4 <- ga_model_shiny_load("{{ model4 }}")
model5 <- ga_model_shiny_load("{{ model5 }}")

## ui.R
ui <- fluidPage(title = "{{ shiny_title }}",
                authDropdownUI("auth_menu"),
                h2("Model Description"),
                textOutput("model_description"),
                h2("Model Output"),
                model1$ui("model1"),
                model2$ui("model2"),
                model3$ui("model3"),
                model4$ui("model4"),
                model5$ui("model5")
                
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
  
  # The model descriptions
  output$model_description <- renderText({
    paste(model1$description,
      model2$description,
      model3$description,
      model4$description,
      model5$description, collapse = "\n ")
    })
  
  # module to display model results
  model1$server("model1", view_id = view_id)
  model2$server("model2", view_id = view_id)
  model3$server("model3", view_id = view_id)
  model4$server("model4", view_id = view_id)
  model5$server("model5", view_id = view_id)
  
}

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
