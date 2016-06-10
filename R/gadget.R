gadget_GASegment <- function(inputValue1, inputValue2) {
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Google Analytics v4 Segment Builder"),
    miniUI::miniContentPanel(
      shiny::textInput("output_var", "Output Variable Name", value = "segment_ga4"),
      segmentBuilderUI("test1")
    )
  )
  
  server <- function(input, output, session) {
    
    segmentR <- shiny::callModule(segmentBuilder, "test1")
    
    shiny::observeEvent(input$done, {
      
      segment <- segmentR()
      
      output_var <- input$output_var
      assign(output_var, segment, envir = .GlobalEnv)
      
      shiny::stopApp()
    })
  }
  
  viewer <- shiny::dialogViewer("GAv4 Segment Builder", width = 800, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
}

