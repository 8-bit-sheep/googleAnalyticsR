# gentelella ui.R
library(gentelellaShiny)   # ui theme

gentelellaPage(
  menuItems = sideBarElement(a("Start Again", href="/")),
  title_tag = "GA time normalised pages",
  site_title = a(class="site_title", icon("clock"), span("Time Normalised")),
  footer = "Made with googleAnalyticsR::ga_model_shiny()",

  # shiny UI elements
  h3("Choose GA account"),
  {{ auth_ui }},
  {{{ date_range }}},
  h3("Time Normalised pages"),
  {{{ model_ui }}},
  br()

)

