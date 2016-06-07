#' Create a GAv4 Segment Builder
#' 
#' For use with \link{segmentBuilder}
#' 
#' @export
segmentBuilderUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      segmentElementUI(ns("ui1")),
      segmentChainUI(ns("chain1")),
      shiny::helpText("Simple segments combine their elements as OR, sequence segments use the Element Sequence setting.")
 
      
    )
  )
  
}

#' Create a GAv4 Segment Builder
#' 
#' For use with \link{segmentBuilderUI}
#' 
#' @export
segmentBuilder <- function(input, output, session){
  
  ns <- session$ns
  
  element_inputs <- shiny::callModule(segmentElement, "ui1")
  
  segment_sequence <- shiny::callModule(segmentChain, "chain1", 
                                        element_inputs = element_inputs)
  
}

segment_sequence_ui <- function(segment_sequence){
  
  parse_sequence <- function(ss){
    segment_sequence_part <- segment_sequence[[ss]]
    
    make_output <- function(id){
      seq1 <- segment_sequence_part[[id]]
      segment_element_ui(id, 
                         seq1, 
                         segment_type = segment_sequence_part[["segment_type"]])
      
    }
    
    out1 <- lapply(names(segment_sequence_part), make_output)
    
    shiny::div(class = "panel panel-default",
               shiny::div(class = "panel-heading", 
                          paste(ss, segment_sequence_part[["segment_type"]])),
               out1)
  }
  
  shiny::tagList(
    lapply(names(segment_sequence), parse_sequence)
  )
}


segment_element_ui <- function(id, seq, segment_type=NULL){

  exps <- NULL
  
  ## UI not fully loaded
  if(length(seq) < 9) return(NULL)
  
  if(seq[["type"]] == "metric"){
    class <- "label label-warning"
    
    exps <- paste(seq[["compValue"]],
                  "[", tolower(seq[["scope"]]), "]")
    
    if(seq[["operator"]] == "BETWEEN"){
      exps <- paste(exps, seq[["maxCompValue"]])
    }
    
  } else { ## dimension
    class <- "label label-primary"
    
    if(seq[["case_sensitive"]]){
      cs <- " (case sensitive)"
    } else {
      cs <- " (not case sensitive)"
    }

    exps <- paste(seq[["expressions"]],
                  cs
                  )
    
    if(seq[["operator"]] == "NUMERIC_BETWEEN"){
      exps <- paste(exps, seq[["minCompValue"]], seq[["maxCompValue"]])
    }

  }
  
  if(seq[["not"]]){
    exclude <- "Exclude: "
  } else {
    exclude <- NULL
  }
  
  if(!is.null(segment_type) && segment_type == "sequence"){
    
    if(!is.null(seq[["matchType"]]) && seq[["matchType"]] == "PRECEDES"){
      seperator <- " > "
    } else {
      seperator <- " >> "
    }
  } else {
    seperator <- " OR "
  }
  
  shiny::tags$li(paste(id, 
                  exclude,
                  seq[["name"]],
                  seq[["operator"]],
                  exps,
                  seperator),
           class = class)

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
    shiny::uiOutput(ns("chain_text")),
    shiny::uiOutput(ns("segment_chain_sequence")),
    shiny::uiOutput(ns("segment_u_s"))
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
    
    segment_length <- shiny::reactiveValues(i=1)
    shiny::observeEvent(element_inputs$submit(), {
      
      sv <- shiny::reactiveValuesToList(segment_vector)
      position <- segment_length$i
      
      segment_length$i <- position + 1
      position <- as.character(position)
      
      ## add to reactive vector segment_chain at moment submit button pressed
      segment_vector[[position]] <- shiny::isolate(segment_chain())
      segment_vector$segment_type <- shiny::isolate(element_inputs$sequence_type())
      segment_vector
      
    })
    

    
    segment_definition <- shiny::reactiveValues()
    segment_definition_length <- shiny::reactiveValues(i=1)
    
    segment_u_s <- shiny::reactiveValues()
    
    shiny::observeEvent(element_inputs$submit_segment_vector(), {
      
      sv <- shiny::reactiveValuesToList(segment_vector)
      
      position <- segment_definition_length$i
      segment_length$i <- as.character(as.numeric(position) + 1)
      position <- as.character(position)
      
      ## add to reactive vector segment_chain at moment submit button pressed
      segment_definition[[position]] <- shiny::isolate(sv)
      
      segment_definition_length$i <- as.character(as.numeric(position) + 1)
      
      ## if this is pressed, reset the segment_vector
      lapply(seq_along(sv), function(x) {
        segment_vector[[as.character(x)]] <- NULL
      })
      
      segment_length$i <- 1
      
      ## add to segment def
      sd <- shiny::reactiveValuesToList(segment_definition)
      
      if(element_inputs$user_or_session() == "user"){
        
        segment_u_s$user <-shiny::isolate(sd)
        
      } else if(element_inputs$user_or_session() == "session"){
        
        segment_u_s$session <-shiny::isolate(sd)
        
      } else {
        warning("Nothing happened to segment_u_s")
      }
      
      segment_u_s
    })
    
    ## the current setting
    output$chain_text <- shiny::renderUI({
      shiny::validate(
        shiny::need(segment_chain(), "Getting chain")
      )
      chain <- segment_chain()
      
      segment_element_ui(id = NULL, seq = chain)
    })
    
    
    ##### create the segment sequences from segment_chain
    output$segment_chain_sequence <- shiny::renderUI({
      
      segment_sequence <- shiny::reactiveValuesToList(segment_vector)
      sequence_type <- element_inputs$sequence_type()
      
      make_output <- function(id){
        seq <- segment_sequence[[id]]
        segment_element_ui(id, seq, segment_type = sequence_type)
      }
      
      out <- lapply(names(segment_sequence), make_output)
      
      shiny::tagList(
        shiny::div(class = "panel panel-default",
                   shiny::div(class = "panel-heading", paste(sequence_type)),
                   out
        )
        
      )
      
    })
    
    output$segment_u_s <- shiny::renderUI({
      
      segment1 <- shiny::reactiveValuesToList(segment_u_s)
      
      shiny::tagList(
        shiny::h2("User"),
        segment_sequence_ui(segment1$user),
        shiny::h2("Session"),
        segment_sequence_ui(segment1$session)
      )

      
    })
    
    return(segment_u_s)
    
}



#' A GAv4 segment element row
#' 
#' 
segmentElementUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$div(
      shiny::fluidRow(
        shiny::column(width = 4,
                      shiny::radioButtons(ns("type"), "Filter Type", 
                                          choices = c(Metric = "metric", 
                                                      Dimension = "dimension"), 
                                          inline = TRUE)
        ),
        shiny::column(width = 4,
                      shiny::radioButtons(ns("not"), 
                                          "Exclude?", 
                                          choices = c(Include = FALSE, 
                                                      Exclude = TRUE), 
                                          inline = TRUE)
        ),
        shiny::column(width = 4,
                      shiny::radioButtons(ns("sequence_type"), "Segment Type",
                                          choices = c(Simple = "simple", Sequence = "sequence"), 
                                          inline = TRUE)
        )
        
      ),
      shiny::fluidRow(
        
        shiny::column(width = 4,
                      shiny::uiOutput(ns("dynamic_names"))
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
        ),
        shiny::column(width = 4,
                      shiny::actionButton(ns("submit_segment_vector"),
                                          "Add Segment Vector", 
                                          icon = shiny::icon("plus-square-o"),
                                          class = "btn btn-success")
                      ),
        shiny::column(width = 4,
                      shiny::uiOutput(ns("matchType_ui")),
                      shiny::radioButtons(ns("user_or_session"), "User Or Session",
                                          choices = c("user", "session"),
                                          inline = TRUE)
                      )
      ) 
    , class = "well"  
    )
  )
}

segmentElement <- function(input, output, session){
  
  ns <- session$ns
  
  output$matchType_ui <- shiny::renderUI({
    shiny::validate(
      shiny::need(input$sequence_type, "sequence_type")
    )
    
    sequence_type <- input$sequence_type
    
    if(sequence_type == "sequence"){
      out <- shiny::radioButtons(ns("matchType"),
                                 "Element Sequence",
                                 c("precedes (>)" = "PRECEDES",
                                   "immediately precedes (>>)" = "IMMEDIATELY_PRECEDES"))
    } else {
      out <- NULL
    }
    
    out
    

  })
  
  output$dynamic_names <- shiny::renderUI({
    shiny::validate(
      shiny::need(input$type, "Type")
    )
    
    type <- input$type
    
    if(type == "metric"){
      
      choices <- meta[meta$type == "METRIC" & meta$status == "PUBLIC", "name"]
      
    } else {
      
      choices <- meta[meta$type == "DIMENSION" & meta$status == "PUBLIC", "name"]
      
    }
    
    shiny::selectInput(ns("name"), type, choices = choices)
    
  })
  
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
      
      choice <- c("<" = "LESS_THAN",
                  ">" = "GREATER_THAN",
                  "=" = "EQUAL",
                  "between" = "BETWEEN")
      
    } else if(type == "dimension"){
      
      choice <- c(
        "regex" = "REGEXP",
        "begins" = "BEGINS_WITH",
        "ends" = "ENDS_WITH",
        "contains" = "PARTIAL",
        "exact" = "EXACT",
        "list" = "IN_LIST",
        "<" = "NUMERIC_LESS_THAN",
        ">" = "NUMERIC_GREATER_THAN",
        "between" = "NUMERIC_BETWEEN"
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
    submit = shiny::reactive(input$submit),
    submit_segment_vector = shiny::reactive(input$submit_segment_vector),
    sequence_type = shiny::reactive(input$sequence_type),
    user_or_session = shiny::reactive(input$user_or_session)
  ))

}
