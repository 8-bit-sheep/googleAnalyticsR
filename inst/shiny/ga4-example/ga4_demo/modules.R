# #' multi_select UI
# #'
# #' Shiny Module for use with \link{metric_select}
# #' 
# #' @param id Shiny id
# #' @param label label
# #' @param multiple
# #'
# #' @return Shiny UI
# multi_selectUI <- function(id, 
#                            label = "Metric",
#                            multiple = TRUE,
#                            width = NULL){
# 
#   ns <- shiny::NS(id)
#   
#   selectInput(ns("multi_select"),
#               label=label,
#               choices = NULL,
#               multiple = multiple,
#               width = width)
# 
# }
# 
# #' multi_select
# #'
# #' Shiny Module for use with \link{metric_selectUI}
# #'
# #' Call via \code{shiny::callModule(metric_select, "your_id")}
# #'
# #' @param input shiny input
# #' @param output shiny output
# #' @param session shiny session
# #' @param type metric or dimension
# #'
# #' @return the selected variable
# multi_select <- function(input, output, session, 
#                          type = c("METRIC","DIMENSION"),
#                          subType = c("none","segment","cohort")){
# 
#   type <- match.arg(type)
#   
#   ns <- session$ns
#   
#   ## update select from meta
#   observe({
#     
#     choice <- allowed_metric_dim(type = type, subType = subType)
#     
#     updateSelectInput(session,
#                       "multi_select",
#                       choices = choice,
#                       selected = choice[1])
#   })
#   
#   return(shiny::reactive(input$multi_select))
# 
# }