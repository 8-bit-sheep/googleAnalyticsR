library(shiny)
library(googleAuthR)
gar_set_client()

fileSearch <- function(query) {
  googleAuthR::gar_api_generator("https://www.googleapis.com/drive/v3/files/",
                                 "GET",
                                 pars_args=list(q=query),
                                 data_parse_function = function(x) x$files)()
}

## ui.R
ui <- fluidPage(title = "googleAuthR Shiny Demo",
                textInput("query", 
                          label = "Google Drive query", 
                          value = "mimeType != 'application/vnd.google-apps.folder'"),
                tableOutput("gdrive")
)

## server.R
server <- function(input, output, session){
  
  # this is not reactive, no need as you only reach here authenticated
  gar_shiny_auth(session)
  
  output$gdrive <- renderTable({
    req(input$query)
    
    # no need for with_shiny()
    fileSearch(input$query)
    
  })
}

# gar_shiny_ui() needs to wrap the ui you have created above.
shinyApp(gar_shiny_ui(ui), server)