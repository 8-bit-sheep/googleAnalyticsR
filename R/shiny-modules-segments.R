#' Create a GAv4 Segment Builder
#' 
#' Shiny Module for use with \link{segmentBuilder}
#' 
#' @param id Shiny id
#' 
#' @return Shiny UI for use in app
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(shiny)
#' library(googleAnalyticsR)
#' 
#' ui <- shinyUI(fluidPage(
#'                segmentBuilderUI("test1")
#'                ))
#'                
#'                
#' server <- shinyServer(function(input, output, session) {
#' 
#' segment <- callModule(segmentBuilder, "test1")
#' 
#'   .. use segment() in further gav4 calls.
#'   
#' })
#' 
#' 
#' # Run the application 
#' shinyApp(ui = ui, server = server)
#' }
#' @export
segmentBuilderUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      segmentElementUI(ns("ui1")),
      segmentChainUI(ns("chain1"))
    )
  )
  
}

#' Create a GAv4 Segment Builder
#' 
#' Shiny Module for use with \link{segmentBuilderUI}
#' 
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' 
#' @details 
#'
#' Call via \code{shiny::callModule(segmentBuilder, "your_id")}
#' 
#' @return A segment definition
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(shiny)
#' library(googleAnalyticsR)
#' 
#' ui <- shinyUI(fluidPage(
#'                segmentBuilderUI("test1")
#'                ))
#'                
#'                
#' server <- shinyServer(function(input, output, session) {
#' 
#' segment <- callModule(segmentBuilder, "test1")
#' 
#'   .. use segment() in further gav4 calls.
#'   
#' })
#' 
#' 
#' # Run the application 
#' shinyApp(ui = ui, server = server)
#' }
#' 
#' @export
segmentBuilder <- function(input, output, session){
  
  element_inputs <- shiny::callModule(segmentElement, "ui1")
  
  segment_sequence <- shiny::callModule(segmentChain, "chain1", 
                                        element_inputs = element_inputs)
  
  return(segment_sequence)
  
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
  
  if(seq[["type"]] == "METRIC"){
    class <- "label label-warning"
    
    exps <- paste(seq[["comparisonValue"]],
                  "[", tolower(seq[["scope"]]), "]")
    
    if(seq[["operator"]] == "BETWEEN"){
      exps <- paste(exps, seq[["maxComparisonValue"]])
    }
    
  } else { ## dimension
    class <- "label label-primary"
    
    if(!is.null(seq[["caseSensitive"]]) && seq[["caseSensitive"]] == TRUE){
      cs <- " (case sensitive)"
    } else {
      cs <- " (not case sensitive)"
    }
    
    exps <- paste(seq[["expressions"]],
                  cs
    )
    
    if(seq[["operator"]] == "NUMERIC_BETWEEN"){
      exps <- paste(exps, seq[["minComparisonValue"]], seq[["maxComparisonValue"]])
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
#' @keywords internal
segmentChainUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::helpText("The current configuration of segment element."),
    shiny::uiOutput(ns("chain_text")),
    shiny::hr(),
    shiny::helpText("The current simple or sequence vector of segment elements."),
    shiny::uiOutput(ns("segment_chain_sequence")),
    shiny::hr(),
    shiny::helpText("Within sub-segments combine as logical AND"),
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
#' @keywords internal
segmentChain <- function(input, output, session,
                         element_inputs){
  
  ns <- session$ns
  
  ## keeps list of all elements added
  segment_vector <- shiny::reactiveValues()
  ## remembers length of segment_vector
  segment_length <- shiny::reactiveValues(i=1)
  
  ## segment_vectors per user or session scope
  segment_definition_user <- shiny::reactiveValues()
  segment_definition_session <- shiny::reactiveValues()
  segment_definition_length <- shiny::reactiveValues(u=1, s=1)    
  
  ## a simple or sequence segment element chain
  segment_chain <- shiny::reactive({
    
    chain <- character(1)
    
    for(i in seq_along(element_inputs)){
      
      inputA <- element_inputs[[i]]()
      
      if(is.null(inputA)) next
      
      names(inputA) <- names(element_inputs[i])
      chain <- c(chain, inputA)
    }
    
    expected <- list(
      type = "",
      not = "",
      matchType = "",
      name = "",
      operator = "",
      minComparisonValue = "",
      maxComparisonValue = "",
      expressions = "",
      caseSensitive = "",
      comparisonValue = "",
      scope = "",
      submit = "",
      submit_segment_vector = "",
      sequence_type = "",
      user_or_session = "",
      segment_name = "")
    
    ## add NULLs for those not present expected metrics
    chain <- lapply(names(expected), 
                    function(x) if(!x %in% names(chain)) NULL else chain[[x]])
    names(chain) <- names(expected)
    
    
    chain
    
  })
  
  
  ## upon Add segment element, add the segment chain to position X
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
  
  
  segment_u_s <- shiny::reactiveValues()
  
  shiny::observeEvent(element_inputs$submit_segment_vector(), {
    
    sv <- shiny::reactiveValuesToList(segment_vector)
    
    if(element_inputs$user_or_session() == "user"){
      
      position <- as.character(segment_definition_length$u)
      segment_definition_length$u <- as.numeric(position) + 1
      
      ## add to reactive vector segment_chain at moment submit button pressed
      segment_definition_user[[position]] <- shiny::isolate(sv)
      
      ## add to user segment definition
      segment_u_s$user <-shiny::isolate(shiny::reactiveValuesToList(segment_definition_user))
      
    } else if(element_inputs$user_or_session() == "session"){
      
      position <- as.character(segment_definition_length$s)
      segment_definition_length$s <- as.numeric(position) + 1
      
      ## add to reactive vector segment_chain at moment submit button pressed
      segment_definition_session[[position]] <- shiny::isolate(sv)
      
      ## add to session segment definition
      segment_u_s$session <-shiny::isolate(shiny::reactiveValuesToList(segment_definition_session))
      
    }
    
    ## reset segment vector
    segment_length$i <- 1
    
    lapply(seq_along(sv), function(x) {
      segment_vector[[as.character(x)]] <- NULL
    })
    
    
  })
  
  ## the current setting
  output$chain_text <- shiny::renderUI({
    shiny::validate(
      shiny::need(segment_chain(), "Getting chain")
    )
    chain <- segment_chain()
    
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(width = 10, offset = 1,
                      shiny::div(class = "text-left",
                                 shiny::strong(" Current Element : "),
                                 segment_element_ui(id = NULL, seq = chain)         
                      )
        ) 
        
      )
      
    )
    
  })
  
  
  ##### create the segment sequences from segment_chain
  output$segment_chain_sequence <- shiny::renderUI({
    shiny::validate(
      shiny::need(element_inputs$sequence_type(), "element_inputs$sequence_type()")
    )
    
    segment_sequence <- shiny::reactiveValuesToList(segment_vector)
    sequence_type <- element_inputs$sequence_type()
    
    make_output <- function(id){
      seq <- segment_sequence[[id]]
      segment_element_ui(id, seq, segment_type = sequence_type)
    }
    
    out <- lapply(names(segment_sequence), make_output)
    
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(width = 10, offset = 1,
                      shiny::div(class = "panel panel-default",
                                 shiny::div(class = "panel-heading", 
                                            shiny::strong("Current Sequence"),
                                            paste("type: ", sequence_type)),
                                 out
                      )
        )
      )
    )
    
  })
  
  output$segment_u_s <- shiny::renderUI({
    
    segment <- shiny::reactiveValuesToList(segment_u_s)
    
    user_ui <- segment_sequence_ui(segment$user)
    session_ui <- segment_sequence_ui(segment$session)
    
    if(length(user_ui[[1]]) > 0 | length(session_ui[[1]]) > 0){
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(width = 12,
                        shiny::div(class = "panel panel-default",
                                   shiny::div(class = "panel-heading", 
                                              shiny::h2("User sub-segment"),
                                              if(!is.null(user_ui)) user_ui else shiny::br()
                          ))
          ),
          shiny::column(width = 12,
                        shiny::div(class = "panel panel-default",
                                   shiny::div(class = "panel-heading", 
                                              shiny::h2("Session sub-segment"),
                                              if(!is.null(session_ui)) session_ui else shiny::br()
                                   ))
          )
        )
      )
      
    } else {
      shiny::br()
    }
    

    
  })
  
  segmentga4 <- shiny::reactive({
    
    segment1 <- shiny::reactiveValuesToList(segment_u_s)
    
    user_s <- segment_define(unname(lapply(segment1$user, segment_vector_calls)))
    session_s <- segment_define(unname(lapply(segment1$session, segment_vector_calls)))
    
    segment_ga4(
      name = element_inputs$segment_name(),
      user_segment = if(length(user_s$segmentFilters) > 0) user_s else NULL,
      session_segment = if(length(session_s$segmentFilters) > 0) session_s else NULL
    )
    
    
  })
  
  return(segmentga4)
  
}

## helper to turn segment elements into v4 object
segment_element_calls <- function(se){
  
  if(class(se) != "list") return(se)
  
  se_list <- list(name = unname(se[["name"]]),
                  operator = se$operator,
                  type = se$type,
                  not = as.logical(se$not),
                  expressions = se$expressions,
                  caseSensitive = as.logical(se$caseSensitive),
                  minComparisonValue = se$minComparisonValue,
                  maxComparisonValue = se$maxComparisonValue,
                  scope = se$scope,
                  comparisonValue = as.numeric(se$comparisonValue),
                  matchType = se$matchType)
  
  out <- do.call(segment_element, args = se_list)
}

## helper to turn segment lists with segment elements into v4 object
segment_vector_calls <- function(sv){
  
  
  sv1 <- lapply(sv, segment_element_calls)
  
  st <- sv1[["segment_type"]]
  sv1[["segment_type"]] <- NULL
  sv1 <- unname(sv1)
  
  if(st == "simple"){
    segment_vector_simple(list(sv1))
  } else {
    segment_vector_sequence(list(sv1))
  }
  
}


#' A GAv4 segment element row
#' 
#' @param id shiny id
#' 
#' @keywords internal
segmentElementUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$div(
      shiny::fluidRow(
        shiny::column(width = 6,
                      shiny::textInput(ns("segment_name"), 
                                       label = "Segment Name", 
                                       value = "segment1")
        ),
        shiny::column(width = 6,
                      # shiny::helpText("Simple segments combine their elements as OR, sequence segments use the Element Sequence setting."),
                      shiny::br()
        )
        
      ),
      shiny::fluidRow(
        shiny::column(width = 4,
                      shiny::radioButtons(ns("type"), "Filter Type", 
                                          choices = c(Metric = "METRIC", 
                                                      Dimension = "DIMENSION"), 
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
                      shiny::actionButton(ns("submit"), "Add Element", 
                                          icon = shiny::icon("plus-square-o"),
                                          class = "btn btn-success")           
        ),
        shiny::column(width = 4,
                      shiny::actionButton(ns("submit_segment_vector"),
                                          "Add Vector", 
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
    choices <- allowed_metric_dim(type, "segment")
    
    shiny::selectInput(ns("name"), type, choices = choices)
    
  })
  
  output$dynamic_UI <- shiny::renderUI({
    shiny::validate(
      shiny::need(input$type,"Type"),
      shiny::need(input$operator, "Operator")
    )
    
    type <- input$type
    operator <- input$operator
    
    if(type == "DIMENSION"){
      
      if(operator == "NUMERIC_BETWEEN"){
        cvalue <- shiny::tagList(
          shiny::numericInput(ns("minComparisonValue"), "Minimum", 0),
          shiny::numericInput(ns("maxComparisonValue"), "Maximum", 1)
        )
      } else {
        cvalue <-  shiny::tagList(
          shiny::textInput(ns("expressions"), "Expression")
        )
      }
      
      if(!grepl("^NUMERIC_|IN_LIST",operator)){
        cs <- shiny::tagList(
          shiny::radioButtons(ns("caseSensitive"), 
                              "Case Sensitive?", 
                              choices = c(Yes = TRUE,
                                          No = FALSE),
                              inline = TRUE)
        )
        
        out <- c(cvalue, cs)
      } else {
        out <- cvalue
      }
      
    } else if(type == "METRIC"){
      
      out <- shiny::tagList(
        
        shiny::numericInput(ns("comparisonValue"),
                            "Value",
                            0),
        shiny::selectInput(ns("scope"), 
                           "Scope",
                           choices = c("USER",
                                       "SESSION",
                                       "HIT",
                                       "PRODUCT"))
      )
      
      if(operator == "BETWEEN"){
        cvalue <- shiny::tagList(
          shiny::numericInput(ns("maxComparisonValue"), "Maximum", 1)
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
    
    if(type == "METRIC"){
      
      choice <- c(">" = "GREATER_THAN",
                  "<" = "LESS_THAN",
                  "=" = "EQUAL",
                  "between" = "BETWEEN")
      
    } else if(type == "DIMENSION"){
      
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
    minComparisonValue = shiny::reactive(input$minComparisonValue),
    maxComparisonValue = shiny::reactive(input$maxComparisonValue),
    expressions = shiny::reactive(input$expressions),
    caseSensitive = shiny::reactive(input$caseSensitive),
    comparisonValue = shiny::reactive(input$comparisonValue),
    scope = shiny::reactive(input$scope),
    submit = shiny::reactive(input$submit),
    submit_segment_vector = shiny::reactive(input$submit_segment_vector),
    sequence_type = shiny::reactive(input$sequence_type),
    user_or_session = shiny::reactive(input$user_or_session),
    segment_name = shiny::reactive(input$segment_name)
  ))
  
}