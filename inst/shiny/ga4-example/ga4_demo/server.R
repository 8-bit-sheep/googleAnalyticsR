library(shiny)
library(googleAuthR)
library(googleAnalyticsR)
library(listviewer)

## this is set before you launch the app, 
## as the default local port is 1221
## change this to the port you use if on your own Google project keys
options(shiny.port = 1221)
options(googleAuthR.webapp.client_id = "289759286325-42j8nmkeq5n9v9eb1kiuj2i97v9oea1f.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "0zBtmZ_klTEzXUaTUTP5AkNQ")
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics.readonly"))

shinyServer(function(input, output, session){
  
  #####--------- Setup
  
  token <- callModule(googleAuth, "login")
  
  ga_accounts <- reactive({
    validate(
      need(token(), "Authenticate")
    )
    
    with_shiny(ga_account_list, shiny_access_token = token())
  })
  
  selected_id <- callModule(authDropdown, "auth_menu", ga.table = ga_accounts)
  
  #####--------- Segments
  
  segment_built <- callModule(segmentBuilder, "demo_segments")
  
  segment_metrics <- callModule(multi_select, "metric_seg", type = "METRIC", subType = "segment")
  segment_dims <- callModule(multi_select, "dim_seg", type = "DIMENSION", subType = "segment")
  
  segment_data <- eventReactive(input$get_seg, {
    
    viewId <- selected_id()
    metrics <- segment_metrics()
    dims <- c("ga:segment", segment_dims())
    dates <- input$date_seg
    segment_built <- segment_built()
    
    with_shiny(google_analytics,
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
  
  output$segment_object <- renderJsonedit({
    
    jsonedit(
      as.list( segment_built() )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }')
    )
    
    
    
})
  
  #####--------- Cohorts
  
  cohort_metrics <- callModule(multi_select, "metric_coh", 
                               type = "METRIC", 
                               subType = "cohort")
  
  cohort_dims <- callModule(multi_select, "dim_coh", 
                            type = "DIMENSION", 
                            subType = "cohort", 
                            default = "ga:cohort")
  
  cohort_built <- reactive({
    
    coh_d1 <- as.character(input$date_coh1)
    coh_d2 <- as.character(input$date_coh2)    
    coh_d3 <- as.character(input$date_coh3)
    coh_d4 <- as.character(input$date_coh4)
    coh1 <- input$coh1
    coh2 <- input$coh2
    coh3 <- input$coh3
    coh4 <- input$coh4
    
    c_list <- list(c(coh_d1[1], coh_d1[2]),
                   c(coh_d2[1], coh_d2[2]),
                   c(coh_d3[1], coh_d3[2]),
                   c(coh_d4[1], coh_d4[2]))
    names(c_list) <-  c(coh1, coh2, coh3, coh4)
    
    make_cohort_group(c_list)
    
  })
  
  cohort_data <- eventReactive(input$get_cohort, {
    
    viewId <- selected_id()
    metrics <- cohort_metrics()
    dims <- cohort_dims()
    cohort_built <- cohort_built()
    
    with_shiny(google_analytics,
               shiny_access_token = token(),
               viewId = viewId,
               metrics = metrics,
               dimensions = dims,
               cohort = cohort_built)
    
  })
  
  output$cohort_table <- renderDataTable({
    
    cohort_data()
    
  })
  
  output$cohort_object <- renderJsonedit({
    
    jsonedit(
      as.list( cohort_built() )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }')
    )
    
    
    
})
  
  #####--------- Multi-date
  
  md_metrics <- callModule(multi_select, "metric_md", type = "METRIC", subType = "all")
  md_dims <- callModule(multi_select, "dim_md", type = "DIMENSION", subType = "all")
  
  md_data <- eventReactive(input$get_md, {
    
    viewId <- selected_id()
    metrics <- md_metrics()
    dims <- md_dims()
    dates1 <- input$date1_md
    dates2 <- input$date2_md
    
    with_shiny(google_analytics,
               shiny_access_token = token(),
               viewId = viewId,
               date_range = c(dates1[1], dates1[2], dates2[1], dates2[2]),
               metrics = metrics,
               dimensions = dims)
    
  })
  
  output$md_table <- renderDataTable({
    
    md_data()
    
  })
  
  md_object <- reactive({
    
    viewId <- selected_id()
    metrics <- md_metrics()
    dims <- md_dims()
    dates1 <- input$date1_md
    dates2 <- input$date2_md
    
    out <- make_ga_4_req(google_analytics,
                         viewId = viewId,
                         date_range = c(dates1[1], dates1[2], dates2[1], dates2[2]),
                         metrics = metrics,
                         dimensions = dims)
    out$dimensionFilterClauses <- NULL
    out
    
  })
  
  output$md_object <- renderJsonedit({
    
    jsonedit(
      as.list( md_object() )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }')
    )
    
    
    
})
  
  #####--------- Pivots 
  
  pv_metrics <- callModule(multi_select, "metric_pivot", type = "METRIC", subType = "all") 
  
  pv_dims <- callModule(multi_select, "dim_pivot", 
                        type = "DIMENSION", 
                        subType = "all", 
                        default = "ga:source")
  
  pv_metrics2 <- callModule(multi_select, "metric_pivot2", type = "METRIC", subType = "all")
  
  pv_dims2 <- callModule(multi_select, "dim_pivot2", 
                         type = "DIMENSION", 
                         subType = "all",
                         default = "ga:medium")
  
  pivot_object <- reactive({
    
    
    pv_metrics2 <- pv_metrics2()
    pv_dims2 <- pv_dims2()
    
    pivot_ga4(pv_dims2, pv_metrics2)
    
  })
  
  pivot_data <- eventReactive(input$get_pivot,{
    
    viewId <- selected_id()
    metrics <- pv_metrics() 
    dims <- pv_dims()
    dates <- input$date_pivot
    pivot_object <- pivot_object()
    
    with_shiny(google_analytics,
               shiny_access_token = token(),
               viewId = viewId,
               date_range = c(dates[1], dates[2]),
               metrics = metrics,
               dimensions = dims,
               pivots = list(pivot_object))
    
    
  })
  
  output$pivot_table <- renderDataTable({
    
    pivot_data()
    
  })
  
  output$pivot_object <- renderJsonedit({
    
    jsonedit(
      as.list( pivot_object() )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }')
    )
    
    
    
})
  
  #####--------- Calculated Metrics
  
  calc_dim <- callModule(multi_select, "dim_calc", type = "DIMENSION", subType = "all")
  calc_met <- callModule(multi_select, "metric_calc", type = "METRIC", subType = "all")
  
  calc_data <- eventReactive(input$get_calc, {
    
    viewId <- selected_id()
    dims <- calc_dim()
    dates <- input$date_clac
    metric_name <- gsub(" ", "", input$calculated_name)
    metric_exp <- input$calculated_exp
    normal_metrics <- calc_met()
    
    exp_metrics <- setNames(metric_exp, metric_name)
    metrics <- c(exp_metrics, normal_metrics)
    
    with_shiny(google_analytics,
               shiny_access_token = token(),
               viewId = viewId,
               date_range = c(dates[1], dates[2]),
               metrics = metrics,
               dimensions = dims)
    
    
  })
  
  output$calc_table <- renderDataTable({
    
    calc_data()
    
  })
  
  calc_object <- reactive({
    
    viewId <- selected_id()
    dims <- calc_dim()
    dates <- input$date_clac
    metric_name <- gsub(" ", "", input$calculated_name)
    metric_exp <- input$calculated_exp
    normal_metrics <- calc_met()
    
    exp_metrics <- setNames(metric_exp, metric_name)
    metrics <- c(exp_metrics, normal_metrics)
    
    make_ga_4_req(viewId = viewId,
                  date_range = c(dates[1], dates[2]),
                  metrics = metrics,
                  dimensions = dims)
    
  })
  
  output$calc_object <- renderJsonedit({
    
    jsonedit(
      as.list( calc_object() )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }')
    )
    
    
    
  })
  
  
  })