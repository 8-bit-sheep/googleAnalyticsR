#' accountPicker UI [Shiny Module] - pick GA4 accounts/webProperties in Shiny
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
accountPickerUI <- function(id, width = NULL, inColumns = FALSE){
  
  ns <- shiny::NS(id)
  
  dropdowns <- list(my_input(ns("account_name"),"Accounts", width),
                    my_input(ns("property_name"),"Select Property", width))
  
  if(inColumns){
    dropdowns <- put_in_cols(dropdowns, 6)
  }
  
  shiny::tagList(dropdowns)
  
}

my_input <- function(id, label, width){
  shiny::selectInput(id,
                     label=label,
                     choices = NULL,
                     width = width)
}

put_in_cols <- function(l, width=4){
  lapply(l, function(x) shiny::column(width=width, x))
}

is.ga4_table <- function(df){
  all(names(df) == c("account_name","accountId","property_name","propertyId"))
}

#' accountPicker [Shiny Module] - pick GA4 accounts/webProperties in Shiny
#'
#' Shiny Module for use with \link{accountPickerUI}
#'
#' Call via \code{accountPicker("your_id", ga_table)}
#'
#' @param ga_table A table GA4 accounts/web properties from \code{ga_account_summary("ga4")}
#' @param id_only Whether to return just the id, not the row
#'
#' @return If id_only=FALSE, the row of ga_table for the selected GA4 web property e.g. use \code{ga_table$propertyId} to send to \link{ga_data} calls.  If id_only=TRUE, just the propertyId
#' 
#' @family Shiny modules
#' @importFrom dplyr select filter pull
#' @export
accountPicker <- function(id, ga_table, id_only = TRUE){
  shiny::moduleServer(
    id,
    function(input, output, session){
      
      shiny::observe({
        shiny::req(ga_table())
        
        ga_table <- ga_table()
        
        if(!is.ga4_table(ga_table)) stop("Not a GA4 account property table")
        
        choice <- unique(ga_table$account_name)
        
        shiny::updateSelectInput(session, 
                                 "account_name",
                                 label="Accounts",
                                 choices = choice)
      })
      
      shiny::observe({
        shiny::req(ga_table())
        
        ga_table <- ga_table()
        
        choice <- ga_table %>% 
          dplyr::filter(account_name == input$account_name) %>%
          dplyr::pull(property_name)
        
        shiny::updateSelectInput(session, 
                                 "property_name",
                                 label="Web Properties",
                                 choices = choice)
      })
      
      
      chosen_view <- shiny::reactive({
        shiny::req(input$property_name)
        ga_table <- ga_table()
        
        o <- ga_table %>% 
          filter(property_name == input$property_name)
        
        if(id_only){
          o <- o %>% pull(propertyId)
        }
        
        o

      })
      
      return(chosen_view)
    }
  )
  
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