
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
                                      .ui_dots){
  assert_that(
    is.function(f)
  )
  
  if(!check_packages_installed("shiny", stop_if_not = FALSE)){
    return()
  }
  
  server_func <- function(input, output, session, selected_id, .server_dots){
    
    gadata <- shiny::reactive({
      shiny::validate(shiny::need(selected_id()))
      
      data_f(selected_id(), ...)
    })
  }
  
  
}

