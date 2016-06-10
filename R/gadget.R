gadget_GASegment <- function(inputValue1, inputValue2) {
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Google Analytics v4 Segment Builder"),
    miniUI::miniContentPanel(
      shiny::textInput("output_var", "Output Variable Name", value = "segment_ga4"),
      segmentBuilderUI("gadget")
    )
  )
  
  server <- function(input, output, session) {
    
    segmentR <- shiny::callModule(segmentBuilder, "gadget")
    
    shiny::observeEvent(input$done, {
      
      segment <- segmentR()
      
      output_var <- input$output_var
      assign(output_var, segment, envir = .GlobalEnv)
      message("Segment saved to variable: ", output_var)
      
      shiny::stopApp()
    })
  }
  
  viewer <- shiny::dialogViewer("GAv4 Segment Builder", 
                                width = 800, height = 1000)
  shiny::runGadget(ui, server, viewer = viewer)
}

