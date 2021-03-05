# use this file to deploy the app online e.g. shinyapps.io
library(shiny)
library(googleAuthR)

ui     <- source("app/ui.R", chdir = TRUE)$value
server <- source("app/server.R", chdir = TRUE)$value

shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
