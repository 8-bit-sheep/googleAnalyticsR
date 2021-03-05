# use this file to deploy the app online e.g. shinyapps.io
library(shiny)
library(googleAuthR)

ui     <- source("ui.R")$value
server <- source("server.R")$value

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)