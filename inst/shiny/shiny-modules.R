#' Create a GAv4 Segment Builder
#' 
#' For use with \link{segmentBuilder}
#' 
#' @export
segmentBuilderUI <- function(id){
  
  ns <- shiny::NS(id)
  
  segmentElementUI(ns("ui1"))
  
}

#' Create a GAv4 Segment Builder
#' 
#' For use with \link{segmentBuilderUI}
#' 
#' @export
segmentBuilder <- function(input, output, session){
  
  element_inputs <- shiny::callModule(segmentElement, "ui1")
  
}

#' A GAv4 segment element row
#' 
#' 
segmentElementUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::radioButtons(ns("type"), "Type", choices = c(Metric = "metric", 
                                                    Dimension = "dimension")),
    ## could make this a multi select with choices from meta API
    shiny::textInput(ns("name"), "Name"),
    shiny::selectInput(ns("operator"), "Operator", choices = NULL),
    shiny::radioButtons(ns("not"), "Exclude?", choices = c(Include = FALSE, 
                                                      Exclude = TRUE)),
    shiny::uiOutput(ns("dynamic_UI")),
    shiny::radioButtons(ns("matchType"),"Match Type", c("PRECEDES","IMMEDIATELY_PRECEDES"))
  )
}

segmentElement <- function(input, output, session){
  
  ns <- session$ns
  
  output$dynamic_UI <- shiny::renderUI({
    shiny::validate(
      shiny::need(input$type,"Type"),
      shiny::need(input$operator, "Operator")
    )
    
    type <- input$type
    operator <- input$operator
    
    if(type == "dimension"){
      out <- shiny::tagList(
        shiny::radioButtons(ns("case_sensitive"), 
                            "Case Sensitive?", 
                            choices = c(Yes = TRUE,
                                        No = FALSE))
      )
      

      if(operator == "NUMERIC_BETWEEN"){
        cvalue <- shiny::tagList(
          shiny::numericInput(ns("minCompValue"), "Minimum", 0),
          shiny::numericInput(ns("maxCompValue"), "Maximum", 1)
        )
      } else {
        cvalue <-  shiny::tagList(
          shiny::textInput(ns("expressions"), "Expression")
        )
      }
      
      out <- c(out, cvalue)
      
    } else if(type == "metric"){
     
      out <- shiny::tagList(
        shiny::selectInput(ns("scope"), 
                           "Scope",
                           choices = c("SESSION",
                                       "USER",
                                       "HIT",
                                       "PRODUCT")),
        shiny::numericInput(ns("compValue"),
                            "Value",
                            0)
      )
      
      if(operator == "BETWEEN"){
        cvalue <- shiny::tagList(
          shiny::numericInput(ns("maxCompValue"), "Maximum", 1)
        )
        
        out <- c(out, cvalue)
      }
        
      
    } else {
      out <- NULL
    }
    
    out

  })

  ## update input$operator
  shiny::observe({
    shiny::validate(
      shiny::need(input$type,"Type")
    )
    
    type <- input$type
    
    if(type == "metric"){
      
      choice <- c("LESS_THAN","GREATER_THAN","EQUAL","BETWEEN")
      
    } else if(type == "dimension"){
      
      choice <- c(
        "REGEXP",
        "BEGINS_WITH",
        "ENDS_WITH",
        "PARTIAL",
        "EXACT",
        "IN_LIST",
        "NUMERIC_LESS_THAN",
        "NUMERIC_GREATER_THAN",
        "NUMERIC_BETWEEN"
      )
    } else {
      choice <- NULL
    }
    
    shiny::updateSelectInput(session, "operator",
                             choices = choice)
    
    
  })
  
  return(input)

}

# segment_element <- function(name,
#                             operator = c("REGEXP",
#                                          "BEGINS_WITH",
#                                          "ENDS_WITH",
#                                          "PARTIAL",
#                                          "EXACT",
#                                          "IN_LIST",
#                                          "NUMERIC_LESS_THAN",
#                                          "NUMERIC_GREATER_THAN",
#                                          "NUMERIC_BETWEEN",
#                                          "LESS_THAN",
#                                          "GREATER_THAN",
#                                          "EQUAL",
#                                          "BETWEEN"),
#                             type = c("metric", "dimension"),
#                             not = FALSE,
#                             expressions=NULL,
#                             caseSensitive=NULL,
#                             minComparisonValue=NULL,
#                             maxComparisonValue=NULL,
#                             scope=c("SESSION","USER","HIT","PRODUCT"),
#                             comparisonValue=NULL,
#                             matchType = c("PRECEDES", "IMMEDIATELY_PRECEDES")){
