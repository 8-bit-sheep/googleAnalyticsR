#' authDropdown UI
#'
#' Shiny Module for use with \link{authDropdown}
#' 
#' @param id Shiny id
#' 
#' @import shiny
#' @return Shiny UI
#' @export
authDropdownUI <- function(id){
  
  ns <- shiny::NS(id)
  
  tagList(
    fluidRow(
      column(
        selectInput(ns("accounts"),
                    label="Accounts",
                    choices = NULL)
        , width = 4, title="Select Account", status="success", solidHeader=TRUE),
      column(
        selectInput(ns("web.prop"),
                    label="WebProperty",
                    choices = NULL)
        , width = 4, title="Select Web Property", status="success", solidHeader=TRUE),
      column(
        selectInput(ns("view"),
                    label="Select View",
                    choices = NULL)
        , width = 4, title="Pick View (ID)", status="success", solidHeader=TRUE)
    )
  )
  
}

#' authDropdown
#'
#' Shiny Module for use with \link{authDropdownUI}
#'
#' Call via \code{shiny::callModule(authDropdown, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param ga.table A table of GA tables
#'
#' @import shiny
#' @export
#' @return GA View Id selected
authDropdown <- function(input, output, session, ga.table){
  
  ns <- session$ns
  
  pList <- reactive({
    ga.table <- ga.table()
    
    ga.table[,c('accountName','webPropertyId','websiteUrl','viewName', 'viewId')]
    
  })
  
  observe({
    validate(
      need(pList(), "Need profiles")
    )
    pList  <- pList()
    
    choice <- unique(pList$accountName)
    
    updateSelectInput(session, 
                      "accounts",
                      label="Accounts",
                      choices = choice)
  })
  
  observe({
    validate(
      need(input$accounts, "Need accounts")
    )
    pList  <- pList()
    
    pList <- pList[input$accounts == pList$accountName,]
    
    choice <- pList$websiteUrl
    
    updateSelectInput(session, 
                      "web.prop", label="WebProperty",
                      choices = choice)
  })
  
  observe({
    validate(
      need(input$web.prop, "Need web")
    )
    pList <- pList()
    
    pList <- pList[input$web.prop == pList$websiteUrl,]
    
    choice <- pList$viewId 
    
    names(choice) <- paste(pList$viewName, pList$viewId)
    
    updateSelectInput(session, "view",
                      label="Views",
                      choices = choice)
  })
  
  chosen_view <- reactive({
    validate(
      need(input$view, "Please login")
    )
    pList <- pList()
    
    out <- pList[input$view == pList$viewId,]

    out$viewId
  })
  
  return(chosen_view)
  
}

#' multi_select UI
#'
#' Shiny Module for use with \link{multi_select}
#' 
#' Create a Google Analytics variable selector
#' 
#' @param id Shiny id
#' @param label label
#' @param multiple
#'
#' @return Shiny UI
#' @export
multi_selectUI <- function(id, 
                           label = "Metric",
                           multiple = TRUE,
                           width = NULL){
  
  ns <- shiny::NS(id)
  
  selectInput(ns("multi_select"),
              label=label,
              choices = NULL,
              multiple = multiple,
              width = width)
  
}

#' multi_select
#'
#' Shiny Module for use with \link{multi_selectUI}
#'
#' Call via \code{shiny::callModule(multi_select, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param type metric or dimension
#'
#' @return the selected variable
#' @export
multi_select <- function(input, output, session, 
                         type = c("METRIC","DIMENSION"),
                         subType = c("none","segment","cohort")){
  
  type <- match.arg(type)
  
  ns <- session$ns
  
  ## update select from meta
  observe({
    
    choice <- allowed_metric_dim(type = type, subType = subType)
    
    updateSelectInput(session,
                      "multi_select",
                      choices = choice,
                      selected = choice[1])
  })
  
  return(shiny::reactive(input$multi_select))
  
}