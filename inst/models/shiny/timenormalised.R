library(shiny)             # R webapps
library(gentelellaShiny)   # ui theme
library(googleAuthR)       # auth login
library(googleAnalyticsR) # get google analytics
library(dplyr)
library(plotly)
library(scales)
library(ggplot2)
library(purrr)

# takes JSON client secrets from GAR_CLIENT_WEB_JSON
# set before calls to googleAnalyticsR to make sure it doesn't use default project.
gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}")

options(googleAuthR.redirect = "{{ deployed_url }}")

model <- ga_model_load(filename = "{{ ga_model }}")
modelUi <- model$shiny_module$ui
modelServer <- model$shiny_module$server

ui <- gentelellaPage(
  menuItems = sideBarElement(a("Start Again", href="/")),
  title_tag = "GA time normalised pages",
  site_title = a(class="site_title", icon("clock"), span("Time Normalised")),
  footer = "Made with googleAnalyticsR::ga_model_shiny()",

  # shiny UI elements
  column(width = 12, authDropdownUI("auth_dropdown", inColumns = TRUE)),
  fluidRow(column(width = 6, numericInput("first_day", "First day minimum pageviews",
                                 value = 2, min=0, max=100)),
         column(width=6, numericInput("total_min_cutoff", "Minimum Total pageviews",
               value = 500, min = 0, max = 1000))),
  fluidRow(column(width = 6, numericInput("days_live", label = "Days Live",
                                          value = 60, min = 10, max = 400)),
           column(width = 6, textInput("page_regex", label = "Page filter regex", value = ".*"))),
  h3("Time Normalised pages"),
  modelUi("{{ ga_model_name }}"),
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

  modelServer("{{ ga_model_name }}",
             view_id = view_id,
             first_day_pageviews_min = reactive(input$first_day),
             total_unique_pageviews_cutoff = reactive(input$total_min_cutoff),
             days_live_range = reactive(input$days_live),
             page_filter_regex = reactive(input$page_regex))

}
# Run the application
shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)

