library(shiny)
library(googleAnalyticsR)

ui <- shinyUI(fluidPage(
   
  segmentBuilderUI("test1")

))


server <- shinyServer(function(input, output, session) {
   
  segment <- callModule(segmentBuilder, "test1")
  str(segment)
  
})

# Run the application 
shinyApp(ui = ui, server = server)