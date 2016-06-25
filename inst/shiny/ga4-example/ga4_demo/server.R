library(shiny)
library(googleAuthR)
library(googleAnalyticsR)

options(shiny.port = 1221)
source("modules.R")

shinyServer(function(input, output, session){
  
  token <- callModule(googleAuth, "login")
  
  ga_accounts <- reactive({
    validate(
      need(token(), "Authenticate")
    )
    
    with_shiny(google_analytics_account_list, shiny_access_token = token())
  })
  
  selected_id <- callModule(authDropdown, "auth_menu", ga.table = ga_accounts)
  
  segment_built <- callModule(segmentBuilder, "demo_segments")
  
  segment_metrics <- callModule(multi_select, "metric_seg", type = "METRIC", subType = "segment")
  segment_dims <- callModule(multi_select, "dim_seg", type = "DIMENSION", subType = "segment")
  
  segment_data <- eventReactive(input$get_seg, {
    
    viewId <- selected_id()
    metrics <- segment_metrics()
    dims <- c("ga:segment", segment_dims())
    dates <- input$date_seg
    segment_built <- segment_built()
    
    with_shiny(google_analytics_4,
               shiny_access_token = token(),
               viewId = viewId,
               date_range = c(dates[1], dates[2]),
               metrics = metrics,
               dimensions = dims,
               segments = segment_built)
    
  })
  
  output$segment_table <- renderDataTable({
    
    segment_data()
    
  })
  
})