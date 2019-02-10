#' Modelling function factory for Google Analytics data
#' 
#' @param gadata The data from an API call
#' @param required_columns What dimensions and metrics are required
#' @param model_func A function that inputs gadata, and outputs a list of assets
#' @param ... Other parameters passed to the model_func
#' 
#' @export
#' @import assertthat
ga_model <- function(gadata, 
                     required_columns, 
                     model_func, 
                     ...,
                     required_packages = NULL){
  
  assert_that(
    is.data.frame(gadata),
    is.character(required_columns),
    is.function(model_func),
    all(names(gadata) %in% required_columns)
  )
  
  check_packages_installed(required_packages)
  
  o <- model_func(gadata, ...)
  
  o
  
}

#' @noRd
#' @import assertthat
model_func_checker <- function(f){
  assert_that(
    is.function(f)
  )
}

#' @noRd
#' @import assertthat
create_shiny_module_funcs <- function(data_f,
                                      model_f, 
                                      .server_dots, 
                                      .ui_dots,
                                      ui_out_func = shiny::textOutput){
  assert_that(
    is.function(data_f),
    is.function(model_f),
    is.list(.server_dots),
    is.list(.ui_dots),
    is.function(ui_out_func)
  )
  
  if(!check_packages_installed("shiny", stop_if_not = FALSE)){
    return()
  }
  
  server_func <- function(input, output, session, view_id, .server_dots){
    
    gadata <- shiny::reactive({
      shiny::validate(shiny::need(selected_id()))
      
      do.call(data_f, args = c(list(view_id = view_id()), 
                               .server_dots))
    })
    
    model_output <- shiny::reactive({
      shiny::validate(shiny::need(gadata()))
      
      do.call(model_f, args = c(list(gadata = gadata()), 
                               .server_dots))
    })
  }
  
  ui_func <- function(id, .ui_dots){
    ns <- shiny::NS(id)
    
    do.call(ui_out_func, args = c(outputId = ns("ui_out_func"), 
                                  .ui_dots))
  }
  
  list(
    shiny_module = list(
      ui = ui_func,
      server = server_func
    )
  )
  
}

