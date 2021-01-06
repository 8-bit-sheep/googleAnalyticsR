#' accountPicker UI [Shiny Module] - pick GA4 accounts/webProperties in Shiny
#' 
#' @description Makes a dropdown row for use for authentication with GA4 web properties.
#' 
#' @param id Shiny id
#' @param width The width of the input
#' @param inColumns Whether to wrap selectInputs in width=4 columns
#'   
#'   
#' @export
#' @rdname accountPicker
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
#' @param ga_table A table GA4 accounts/web properties from \code{ga_account_summary("ga4")}
#' @param id_only Whether to return just the id, not the row
#'
#' @return If id_only=FALSE, the row of ga_table for the selected GA4 web property e.g. use \code{ga_table$propertyId} to send to \link{ga_data} calls.  If id_only=TRUE, just the propertyId
#' 
#' @family Shiny modules
#' @importFrom dplyr select filter pull
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' ui <- fluidPage(title = "Shiny App",
#'                  accountPickerUI("auth_menu", inColumns = TRUE))
#' server <- function(input, output, session){
#'   token <- gar_shiny_auth(session)
#'   
#'   accs <- reactive({
#'     req(token)
#'     ga_account_list("ga4")
#'    })
#'    
#'   # module for authentication
#'   property_id <- accountPicker("auth_menu", ga_table = accs, id_only = TRUE)
#'  }
#'  
#'  shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
#' 
#' }
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

#' metricDimensionSelectUI - GA4 [Shiny Module]
#' 
#' Create a Google Analytics variable selector
#' 
#' @param id Shiny id
#' @param label label
#' @param multiple multiple select
#' @param width width of select
#'
#' @return Shiny UI
#' @rdname metricDimensionSelect
#' @export
metricDimensionSelectUI <- function(
  id, 
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

#' metricDimensionSelect - GA4 [Shiny Module]
#'
#' Shiny Module for use with GA4 metric and dimension fields fetched via \code{ga_meta("ga4")}
#' 
#' @param id The Shiny id
#' @param field_type metric or dimension
#' @param custom_meta Pass a meta field table from \code{ga_meta("ga4")} to get custom fields from GA4 (reactive)
#' @param default The default selected choice. First element if NULL
#'
#' @return the selected variable
#' @family Shiny modules
#' @export
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' # ui.R
#' metricDimensionSelect("mets1")
#' metricDimensionSelect("dims1")
#' 
#' #server.R
#' metrics <- metricDimensionSelect("mets1", "metric")
#' dims <- metricDimensionSelect("dims1", "dimension")
#' 
#' 
#' 
#' # use in app with custom fields
#' #' ui <- fluidPage(title = "Shiny App",
#'                  accountPickerUI("auth_menu", inColumns = TRUE),
#'                  metricDimensionSelectUI("mets1"),
#'                  metricDimensionSelectUI("dims_custom")
#'                  )
#' server <- function(input, output, session){
#'   token <- gar_shiny_auth(session)
#'   
#'   accs <- reactive({
#'     req(token)
#'     ga_account_list("ga4")
#'    })
#'    
#'   # no custom data
#'   metrics <- metricDimensionSelect("mets1")
#'    
#'   # module for authentication
#'   property_id <- accountPicker("auth_menu", ga_table = accs, id_only = TRUE)
#'   
#'   meta <- reactive({
#'       req(property_id())
#'       ga_meta("data", propertyId = property_id())
#'   })
#'  
#'   
#'   # custom data
#'   dims_custom <- metricDimensionSelect("dims_custom", 
#'                                        type = "dimension", 
#'                                        custom_meta = meta())
#'  }
#'  
#'  shinyApp(gar_shiny_ui(ui, login_ui = silent_auth), server)
#'  
#' 
#' 
#' }
metricDimensionSelect <- function(
  id, 
  field_type = c("metric","dimension"),
  custom_meta = NULL,
  default = NULL){
  
  field_type <- match.arg(field_type)
  
  shiny::moduleServer(
    id,
    function(input, output, session){
      
      ## update select from meta
      shiny::observe({

        if(!is.null(custom_meta)){
          fields <- custom_meta
        } else {
          fields <- googleAnalyticsR::meta4
        }
        
        types <- fields %>% dplyr::filter(class == field_type)
        choice <- types %>% dplyr::pull(apiName)
        names(choice) <- types %>% pull(uiName)

        s <- choice[1]

        if(!is.null(default) && all(default %in% choice)){
            s <- default
        }

        shiny::updateSelectInput(session,
                                 "multi_select",
                                 choices = choice,
                                 selected = s)
      })

      return(shiny::reactive(input$multi_select))
    }
  )
  
}