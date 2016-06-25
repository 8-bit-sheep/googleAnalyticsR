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
  
  cohort_metrics <- callModule(multi_select, "metric_coh", type = "METRIC", subType = "cohort")
  cohort_dims <- callModule(multi_select, "dim_coh", type = "DIMENSION", subType = "cohort")
  
  cohort_built <- reactive({
    
    coh_d1 <- as.character(input$date_coh1)
    coh_d2 <- as.character(input$date_coh2)    
    coh_d3 <- as.character(input$date_coh3)
    coh_d4 <- as.character(input$date_coh4)
    coh1 <- input$coh1
    coh2 <- input$coh2
    coh3 <- input$coh3
    coh4 <- input$coh4
    
    c_list <- list(coh1 = c(coh_d1[1], coh_d1[2]),
                   coh2 = c(coh_d2[1], coh_d2[2]),
                   coh3 = c(coh_d3[1], coh_d3[2]),
                   coh4 = c(coh_d4[1], coh_d4[2]))
    setNames(c_list, c(coh1, coh2, coh3, coh4))
    
    make_cohort_group(c_list)
    
  })
  
  cohort_data <- eventReactive(input$get_cohort, {
    
    viewId <- selected_id()
    metrics <- cohort_metrics()
    dims <- cohort_dims()
    cohort_built <- cohort_built()

    with_shiny(google_analytics_4,
               shiny_access_token = token(),
               viewId = viewId,
               metrics = metrics,
               dimensions = dims,
               cohort = cohort_built)
    
  })
  
  output$cohort_table <- renderDataTable({
    
    cohort_data()
    
  })
  
  md_metrics <- callModule(multi_select, "metric_md", type = "METRIC", subType = "all")
  md_dims <- callModule(multi_select, "dim_md", type = "DIMENSION", subType = "all")
  
  md_data <- eventReactive(input$get_md, {
    
    viewId <- selected_id()
    metrics <- md_metrics()
    dims <- md_dims()
    dates1 <- input$date1_md
    dates2 <- input$date2_md
    
    with_shiny(google_analytics_4,
               shiny_access_token = token(),
               viewId = viewId,
               date_range = c(dates1[1], dates1[2], dates2[1], dates2[2]),
               metrics = metrics,
               dimensions = dims)
    
  })
  
  output$md_table <- renderDataTable({
    
    md_data()
    
  })
  
  
})