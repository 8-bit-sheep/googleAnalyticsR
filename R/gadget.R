gadget_GASegment <- function() {
  
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
      
      ## hack around 'assignments to global environment'
      eee <- globalenv()
      ## globalenv()[[input$output_var]] doesn't work(?)
      eee[[output_var]] <- segment
      
      myMessage("Segment saved to variable: ", output_var, level = 3)
      
      shiny::stopApp()
    })
  }
  
  viewer <- shiny::dialogViewer("GAv4 Segment Builder",
                                width = 800, height = 1000)
  # viewer <- shiny::browserViewer()
  shiny::runGadget(ui, server, viewer = viewer)
}

