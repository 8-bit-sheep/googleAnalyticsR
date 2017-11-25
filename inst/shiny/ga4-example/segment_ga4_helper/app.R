library(shiny)
library(googleAnalyticsR)

ui <- shinyUI(fluidPage(
   
  segmentBuilderUI("test1")

))


server <- shinyServer(function(input, output, session) {
   
  callModule(segmentBuilder, "test1")
  
})

# Run the application 
shinyApp(ui = ui, server = server)