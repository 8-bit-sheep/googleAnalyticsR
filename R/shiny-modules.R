#' Create a GAv4 Segment Builder
#' 
#' For use with \link{segmentBuilder}
#' 
#' @export
segmentBuilderUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    segmentElementUI(ns("ui1")),
    segmentChainUI(ns("chain1"))
  )
  
}

#' Create a GAv4 Segment Builder
#' 
#' For use with \link{segmentBuilderUI}
#' 
#' @export
segmentBuilder <- function(input, output, session){
  
  element_inputs <- shiny::callModule(segmentElement, "ui1")
  
  segment_chain <- shiny::callModule(segmentChain, "chain1", 
                                  element_inputs = element_inputs)
  
  return(segment_chain)
  
}


#' segmentChain UI
#'
#' Shiny Module for use with \link{segmentChain}
#' 
#' @param id Shiny id
#'
#' @return Shiny UI
segmentChainUI <- function(id){

  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::radioButtons(ns("sequence_type"), "Sequence Type", 
                           choices = c(Simple = "simple", Sequence = "sequence")),
    shiny::tableOutput(ns("chain_text")),
    shiny::uiOutput(ns("chain_vector"))
  )


}

#' segmentChain
#'
#' Shiny Module for use with \link{segmentChainUI}
#'
#' Call via \code{shiny::callModule(segmentChain, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param element_inputs from \link{segmentElementUI}
#'
#' @return Something
segmentChain <- function(input, output, session,
                         element_inputs){

    ns <- session$ns
    
    ## keeps list of all elements added
    segment_vector <- shiny::reactiveValues()
    
    shiny::observe({

      segment_vector$segment_type <- input$sequence_type

    })
    
    segment_chain <- shiny::reactive({
      
      chain <- character(1)

      for(i in seq_along(element_inputs)){
        
        inputA <- element_inputs[[i]]()
        
        if(is.null(inputA)) next
        
        names(inputA) <- names(element_inputs[i])
        chain <- c(chain, inputA)
        
      }

      chain

    })
    
    shiny::observeEvent(element_inputs$submit(), {
      
      sv <- shiny::reactiveValuesToList(segment_vector)
      position <- length(sv) + 1
      
      position <- as.character(position)
      
      ## add to reactive vector segment_chain at moment submit button pressed
      segment_vector[[position]] <- shiny::isolate(segment_chain())

    })
    
    ## the current setting
    output$chain_text <- shiny::renderTable({
      chain <- segment_chain()
      
      # paste(names(chain), chain, sep = "=")
      out <- data.frame(as.list(chain))

      out[,setdiff(names(out), c("X..", "submit"))]
    })
    
    ## the sequence vector setting
    chain_vector <- shiny::reactive({
      
      sv <- shiny::reactiveValuesToList(segment_vector)
      
      df_list <- lapply(sv, function(chain){
        out <- data.frame(as.list(chain))
        
        out[,setdiff(names(out), c("X..", "submit"))]
        
      }) 
      
      
    })
    
    output$chain_vector <- shiny::renderUI({
      shiny::validate(
        shiny::need(chain_vector(), "List of df")
      )
      
      sv <- shiny::reactiveValuesToList(segment_vector)
      cv <- sv$segment_type
      df_list <- chain_vector()
      
      length_df <- as.character(1:length(df_list))
      
      
      
      
      
    })
    

    
    return(segment_vector)

}



#' A GAv4 segment element row
#' 
#' 
segmentElementUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(width = 4,
        shiny::radioButtons(ns("type"), "Filter Type", 
                            choices = c(Metric = "metric", 
                                        Dimension = "dimension"), 
                            inline = TRUE)
      ),
      shiny::column(width = 4,
                    shiny::radioButtons(ns("not"), "Exclude?", choices = c(Include = FALSE, 
                                                                           Exclude = TRUE), inline = TRUE)
      ),
      shiny::column(width = 4,
                    shiny::radioButtons(ns("matchType"),"Match Type", c("PRECEDES","IMMEDIATELY_PRECEDES"))
      )
      
    ),
    shiny::fluidRow(

      shiny::column(width = 4,
        shiny::textInput(ns("name"), "Name")
      ),
      shiny::column(width = 4,
        shiny::selectInput(ns("operator"), "Operator", choices = NULL)
      ),  
      shiny::column(width = 4,
        shiny::uiOutput(ns("dynamic_UI"))
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 4,
         shiny::actionButton(ns("submit"), "Add Segment Element", 
                             icon = shiny::icon("plus-square-o"),
                             class = "btn btn-success")           
      )
    )
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
      
      if(!grepl("^NUMERIC_|IN_LIST",operator)){
        cs <- shiny::tagList(
          shiny::radioButtons(ns("case_sensitive"), 
                              "Case Sensitive?", 
                              choices = c(Yes = TRUE,
                                          No = FALSE))
        )
        
        out <- c(cvalue, cs)
      } else {
        out <- cvalue
      }
      
    } else if(type == "metric"){
     
      out <- shiny::tagList(

        shiny::numericInput(ns("compValue"),
                            "Value",
                            0),
        shiny::selectInput(ns("scope"), 
                           "Scope",
                           choices = c("SESSION",
                                       "USER",
                                       "HIT",
                                       "PRODUCT"))
      )
      
      if(operator == "BETWEEN"){
        cvalue <- shiny::tagList(
          shiny::numericInput(ns("maxCompValue"), "Maximum", 1)
        )
        
        out <- c(cvalue, out)
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
  
  return(list(
    type = shiny::reactive(input$type),
    not = shiny::reactive(input$not),
    matchType = shiny::reactive(input$matchType),
    name = shiny::reactive(input$name),
    operator = shiny::reactive(input$operator),
    minCompValue = shiny::reactive(input$minCompValue),
    maxCompValue = shiny::reactive(input$maxCompValue),
    expressions = shiny::reactive(input$expressions),
    case_sensitive = shiny::reactive(input$case_sensitive),
    compValue = shiny::reactive(input$compValue),
    scope = shiny::reactive(input$scope),
    submit = shiny::reactive(input$submit)
  ))

}
