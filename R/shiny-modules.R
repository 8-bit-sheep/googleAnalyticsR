#' authDropdown UI
#'
#' Shiny Module for use with \link{authDropdown}
#' 
#' @param id Shiny id
#' @param width The width of the input, e.g. '400px', or '100%'
#' @param inColumns whether to wrap selectInputs in width=4 columns.
#' 
#' @import shiny
#' @return Shiny UI
#' @export
authDropdownUI <- function(id, 
                           width = NULL,
                           inColumns = FALSE){
  
  ns <- shiny::NS(id)
  
  if(inColumns){
    out <- tagList(
      column(width = 4,
             selectInput(ns("accounts"),
                         label="Accounts",
                         choices = NULL,
                         width = width)    
             ),
      column(width = 4,
             selectInput(ns("web.prop"),
                         label="WebProperty",
                         choices = NULL,
                         width = width) 
             ),
      column(width = 4,
             selectInput(ns("view"),
                         label="Select View",
                         choices = NULL,
                         width = width)    
             )
    )
  } else {
    out <- tagList(
      selectInput(ns("accounts"),
                  label="Accounts",
                  choices = NULL,
                  width = width),
      selectInput(ns("web.prop"),
                  label="WebProperty",
                  choices = NULL,
                  width = width),
      selectInput(ns("view"),
                  label="Select View",
                  choices = NULL,
                  width = width)
    )
  }

  out
  
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
#' @param default The default selected choice. First element if NULL
#'
#' @return the selected variable
#' @export
multi_select <- function(input, output, session, 
                         type = c("METRIC","DIMENSION"),
                         subType = c("none","segment","cohort"),
                         default = NULL){
  
  type <- match.arg(type)
  
  ## update select from meta
  observe({
    
    choice <- allowed_metric_dim(type = type, subType = subType)

    s <- choice[1]
    
    if(!is.null(default)){
      
      default <- checkPrefix(default)
      
      if(all(default %in% choice)){
        s <- default
      } else {
        warning("default '", default, "' not in choice")
      }
    }
    
    updateSelectInput(session,
                      "multi_select",
                      choices = choice,
                      selected = s)
  })
  
  return(shiny::reactive(input$multi_select))
  
}