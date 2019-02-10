#' Use a model function created by ga_model_make
#' 
#' @param viewId The GA viewId to operate on
#' @param model A file location of a model object or a model object 
#'   created by \link{ga_model_make}
#' @param ... Other arguments to pass to the model as needed
#' 
#' @export
#' @family GA modelling functions
ga_model <- function(viewId, model, ...){
  
  # a filepath
  if(is.character(model)){
    assert_that(is.readable(model))
    model <- readRDS(model)
  }
  
  assert_that(is.ga_model(model))
  
  check_packages_installed(model$required_packages)
  
  gadata <- model$data_f(viewId, ...)
  
  stopifnot(all(names(gadata) %in% model$required_columns))
  
  model$model_f(gadata, ...)
}

#' Save a created model
#' 
#' @param model model to save
#' @param filename name to save model under
#' 
#' @export
#' @family GA modelling functions
ga_model_save <- function(model, filename = "gamodel.rds"){
  assert_that(
    is.ga_model(model)
  )
  
  saveRDS(model, filename)
}

#' Load a created model
#' 
#' @param filename name to load model from
#' 
#' @export
#' @import assertthat
#' @family GA modelling functions
ga_model_load <- function(filename = "gamodel.rds"){
  
  assert_that(is.readable(filename))
  
  model <- readRDS(filename)
  
  assert_that(is.ga_model(model))
  
  model

}


#' Modelling function factory for Google Analytics data
#' 
#' @param data_f A function that gets the data - 
#'   must have viewId as first argument
#' @param required_columns What dimensions and metrics are required
#' @param model_f A function that inputs gadata, and outputs a list of assets - 
#'   must take data from result of data_f in first argument.
#' @param required_packages The packages needed for data_f and model_f to work
#' @param ui_out_func A shiny UI output function that will display the results of model_f
#' 
#' The passed functions should all have \code{...} to make them flexible 
#'   in what arguments can be added.  Do not have the same argument names in both functions.
#' 
#' @export
#' @import assertthat
#' @family GA modelling functions
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#'  get_model_data <- function(viewId, 
#'                            date_range = c(Sys.Date()- 300, Sys.Date()), 
#'                            ...){
#'    google_analytics(viewId, 
#'                     date_range = date_range, 
#'                     metrics = "sessions", 
#'                     dimensions = "date", 
#'                     max = -1)
#'  }
#' 
#'  decompose_sessions <- function(df, ...){
#'    web_data_ts <- ts(df$sessions, frequency = 7)
#'    d <- decompose(web_data_ts)
#'    list(decom = d, plot = plot(d))
#'  }
#'  
#'  decomp_ga <- ga_model_make(get_model_data,
#'                             required_columns = c("date", "sessions"),
#'                             model_f = decompose_sessions)
#'  
#'  # fetches data and outputs decomposition
#'  ga_model(81416156, decomp_ga)
#'  
#'  # save the model for later
#'  model_location <- "inst/models/decomp_ga.gamodel"
#'  ga_model_save(decomp_ga, filename = model_location)
#'  
#'  # can load model from file
#'  ga_model(81416156, model_location)
#'  
#'  # load model and use again
#'  model2 <- ga_model_load(model_location)
#'  
#'  ga_model(81416156, model2)
#'  
#' }
ga_model_make <- function(data_f, 
                          required_columns, 
                          model_f, 
                          required_packages = NULL,
                          ui_out_func = shiny::textOutput){
  
  assert_that(
    is.function(data_f),
    is.character(required_columns),
    is.function(model_f)
  )
  
  if(any(setdiff(names(formals(data_f)),"...") %in% names(formals(model_f)))){
    stop("data_f() and model_f() functions should not have same argument names",
         call. = FALSE)
  }
  
  if(!any(names(formals(data_f)) == "...")){
    stop("data_f() arguments need to include ...", call.=FALSE)
  }
  
  if(!any(names(formals(model_f)) == "...")){
    stop("model_f() arguments need to include ...", call.=FALSE)
  }
  
  structure(
    list(
      data_f = data_f,
      required_columns = required_columns,
      model_f = model_f,
      required_packages = required_packages,
      shiny_module = create_shiny_module_funcs(data_f = data_f,
                                        model_f = model_f,
                                        ui_out_func = ui_out_func)
    ), class = "ga_model"
  )
  
}

is.ga_model <- function(x){
  inherits(x, "ga_model")
}

#' @noRd
#' @import assertthat
create_shiny_module_funcs <- function(data_f,
                                      model_f, 
                                      ui_out_func = shiny::textOutput){
  assert_that(
    is.function(data_f),
    is.function(model_f),
    is.function(ui_out_func)
  )
  
  server_func <- function(input, output, session, view_id, ...){
    
    gadata <- shiny::reactive({
      shiny::validate(shiny::need(selected_id()))
      
      do.call(data_f, args = c(list(view_id = view_id()), 
                               list(...)))
    })
    
    model_output <- shiny::reactive({
      shiny::validate(shiny::need(gadata()))
      
      do.call(model_f, args = c(list(gadata = gadata()), 
                               list(...)))
    })
  }
  
  ui_func <- function(id, ...){
    ns <- shiny::NS(id)
    
    do.call(ui_out_func, args = c(outputId = ns("ui_out_func"), 
                                  list(...)))
  }
  
  list(
      ui = ui_func,
      server = server_func
  )
  
}

