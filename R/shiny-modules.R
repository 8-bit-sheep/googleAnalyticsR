#' authDropdown UI [Shiny Module]
#' 
#' @description Makes a dropdown row for use for authentication.
#' 
#' @param id Shiny id.
#' @param width The width of the input
#' @param inColumns whether to wrap selectInputs in width=4 columns.
#'   
#'   Shiny Module for use with \link{authDropdown}.
#'   
#'   
#' @return Shiny UI
#' @family Shiny modules
#' @export
#' 
authDropdownUI <- function(id, 
                           width = NULL,
                           inColumns = FALSE){
  
  ns <- shiny::NS(id)
  
  if(inColumns){
    out <- shiny::tagList(
      shiny::column(width = 4,
                    shiny::selectInput(ns("accounts"),
                                       label="Accounts",
                                       choices = NULL,
                                       width = width)    
      ),
      shiny::column(width = 4,
                    shiny::selectInput(ns("web.prop"),
                                       label="WebProperty",
                                       choices = NULL,
                                       width = width) 
      ),
      shiny::column(width = 4,
                    shiny::selectInput(ns("view"),
                                       label="Select View",
                                       choices = NULL,
                                       width = width)    
      )
    )
  } else {
    out <- shiny::tagList(
      shiny::selectInput(ns("accounts"),
                         label="Accounts",
                         choices = NULL,
                         width = width),
      shiny::selectInput(ns("web.prop"),
                         label="WebProperty",
                         choices = NULL,
                         width = width),
      shiny::selectInput(ns("view"),
                         label="Select View",
                         choices = NULL,
                         width = width)
    )
  }
  
  out
  
}

#' authDropdown [Shiny Module]
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
#' @return GA View Id selected
#' 
#' @family Shiny modules
#' @export
authDropdown <- function(input, output, session, ga.table){
  
  pList <- shiny::reactive({
    ga.table <- ga.table()
    
    ga.table[,c('accountName','webPropertyId','websiteUrl','viewName', 'viewId')]
    
  })
  
  shiny::observe({
    shiny::validate(
      shiny::need(pList(), "Need profiles")
    )
    pList  <- pList()
    
    choice <- unique(pList$accountName)
    
    shiny::updateSelectInput(session, 
                      "accounts",
                      label="Accounts",
                      choices = choice)
  })
  
  shiny::observe({
    shiny::validate(
      shiny::need(input$accounts, "Need accounts")
    )
    pList  <- pList()
    
    pList <- pList[input$accounts == pList$accountName,]
    
    choice <- pList$websiteUrl
    
    shiny::updateSelectInput(session, 
                      "web.prop", label="WebProperty",
                      choices = choice)
  })
  
  shiny::observe({
    shiny::validate(
      shiny::need(input$web.prop, "Need web")
    )
    pList <- pList()
    
    pList <- pList[input$web.prop == pList$websiteUrl,]
    
    choice <- pList$viewId 
    
    names(choice) <- paste(pList$viewName, pList$viewId)
    
    shiny::updateSelectInput(session, "view",
                      label="Views",
                      choices = choice)
  })
  
  chosen_view <- shiny::reactive({
    shiny::validate(
      shiny::need(input$view, "Please login")
    )
    pList <- pList()
    
    out <- pList[input$view == pList$viewId,]

    out$viewId
  })
  
  return(chosen_view)
  
}

#' multi_select UI [Shiny Module]
#'
#' Shiny Module for use with \link{multi_select}
#' 
#' Create a Google Analytics variable selector
#' 
#' @param id Shiny id
#' @param label label
#' @param multiple multiple select
#' @param width width of select
#'
#' @return Shiny UI
#' @family Shiny modules
#' @export
multi_selectUI <- function(id, 
                           label = "Metric",
                           multiple = TRUE,
                           width = NULL){
  
  ns <- shiny::NS(id)
  
  shiny::selectInput(ns("multi_select"),
              label=label,
              choices = NULL,
              multiple = multiple,
              width = width)
  
}

#' multi_select [Shiny Module]
#'
#' Shiny Module for use with \link{multi_selectUI}
#'
#' Call via \code{shiny::callModule(multi_select, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param type metric or dimension
#' @param subType Limit selections to those relevant
#' @param default The default selected choice. First element if NULL
#'
#' @return the selected variable
#' @family Shiny modules
#' @export
multi_select <- function(input, output, session, 
                         type = c("METRIC","DIMENSION"),
                         subType = c("all","segment","cohort"),
                         default = NULL){
  
  type <- match.arg(type)
  
  ## update select from meta
  shiny::observe({
    
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
    
    shiny::updateSelectInput(session,
                      "multi_select",
                      choices = choice,
                      selected = s)
  })
  
  return(shiny::reactive(input$multi_select))
  
}