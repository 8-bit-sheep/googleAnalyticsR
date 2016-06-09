gadgetGARequest <- function(inputValue1, inputValue2) {
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Google Analytics v4 Segment Builder"),
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Segments", icon = shiny::icon("object-group"),
        miniUI::miniContentPanel(
          segmentBuilderUI("test1")
        )
      ),
      miniUI::miniTabPanel("Metrics/Dimensions",icon = shiny::icon("sliders"),
        shiny::br()
      )
    )
  )
  
  server <- function(input, output, session) {
    # Define reactive expressions, outputs, etc.
    
    segmentR <- shiny::callModule(segmentBuilder, "test1")
    
    # When the Done button is clicked, return a value
    shiny::observeEvent(input$done, {
      segmentL <- shiny::reactiveValuesToList(segmentR)
      
      str(segmentL)
      
      shiny::stopApp(segmentL)
    })
  }
  
  shiny::runGadget(ui, server)
}

