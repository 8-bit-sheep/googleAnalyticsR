library(shiny)             # R webapps
library(gentelellaShiny)   # ui theme
library(googleAuthR)       # auth login
library(googleAnalyticsR) # get google analytics

# takes JSON client secrets from GAR_CLIENT_WEB_JSON
# set before calls to googleAnalyticsR to make sure it doesn't use default project.
gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

options(googleAuthR.redirect = "{{ deployed_url }}")

# loads a pre-existing models, or NULL if they aren't present
model1 <- ga_model_shiny_load("{{ model1 }}")
model2 <- ga_model_shiny_load("{{ model2 }}")
model3 <- ga_model_shiny_load("{{ model3 }}")
model4 <- ga_model_shiny_load("{{ model4 }}")
model5 <- ga_model_shiny_load("{{ model5 }}")

ui <- gentelellaPage(
  menuItems = sideBarElement(a("Start Again", href="/")),
  title_tag = "GA time normalised pages",
  site_title = a(class="site_title", icon("clock"), span("Time Normalised")),
  footer = "Made with googleAnalyticsR::ga_model_shiny()",

  # shiny UI elements
  h3("Choose GA account"),
  authDropdownUI("auth_dropdown", inColumns = TRUE),
  h3("Time Normalised pages"),
  model1$ui("model1"),
  model2$ui("model2"),
  model3$ui("model3"),
  model4$ui("model4"),
  model5$ui("model5"),
  br()

)

server <- function(input, output, session) {

  token <- gar_shiny_auth(session)

  al <- reactive({
    req(token)
    ga_account_list()
  })

  # module for authentication
  view_id <- callModule(authDropdown, "auth_dropdown", ga.table = al)

  # module to display model results
  model1$server("model1", view_id = view_id)
  model2$server("model2", view_id = view_id)
  model3$server("model3", view_id = view_id)
  model4$server("model4", view_id = view_id)
  model5$server("model5", view_id = view_id)

}
# Run the application
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)

