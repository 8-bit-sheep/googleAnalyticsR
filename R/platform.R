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
    model <- ga_model_load(model)
  }
  
  assert_that(is.ga_model(model))
  
  check_packages_installed(model$required_packages)
  
  gadata <- model$data_f(viewId, ...)
  
  message("Downloaded data.
          Rows:", nrow(gadata),
          " Columns:", paste(names(gadata), collapse = " "))
  
  if(!all(names(gadata) %in% model$required_columns)){
    stop("All required columns were not found in returned data.
         Got: ", paste(names(gadata), collapse=" "),
         " Needed: ", paste(model$required_columns, collapse = " "),
         call. = FALSE)
  }
  
  o <- model$model_f(gadata, ...)
  
  message("Applied model to data, returning its output")
  o
}

#' Save a created model
#'
#' @param model model to save
#' @param filename name to save model under
#'
#' @export
#' @family GA modelling functions
ga_model_save <- function(model, filename = "my-model.gamr"){
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
ga_model_load <- function(filename = "my-model.gamr"){
  
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
#' @param description An optional description of what the model does
#' @param renderShiny A shiny render function that will create the output for outputShiny from model_f
#' @param outputShiny A shiny UI output function that will display the results renderShiny
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
#'                             model_f = decompose_sessions,
#'                             description = "Performs decomposition on session data and creates a plot")
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
#'  # to use in Shiny, supply the output and render functions
#'  myRenderPlot <- function(x){
#'    # base plot needs to plot here, not in model_f as can't pass plot objects
#'    shiny::renderPlot(plot(x$decom))
#'  }
#'
#'  decomp_ga <- ga_model_make(get_model_data,
#'                             required_columns = c("date", "sessions"),
#'                             model_f = decompose_sessions,
#'                             description = "Performs decomposition on session data and creates a plot",
#'                             outputShiny = shiny::plotOutput,
#'                             renderShiny = myRenderPlot)
#'
#' }
ga_model_make <- function(data_f,
                          required_columns,
                          model_f,
                          required_packages = NULL,
                          description = NULL,
                          outputShiny = shiny::plotOutput,
                          renderShiny = function(x) shiny::renderPlot(plot(x))){
  
  assert_that(
    is.function(data_f),
    is.character(required_columns),
    is.function(model_f)
  )
  
  if(any(function_args(data_f) %in% function_args(model_f))){
    stop("data_f() and model_f() functions should not have same argument names",
         call. = FALSE)
  }
  
  if(!any(function_args(data_f, TRUE) == "...")){
    stop("data_f() arguments need to include ...", call.=FALSE)
  }
  
  if(!any(function_args(data_f, TRUE) == "...")){
    stop("model_f() arguments need to include ...", call.=FALSE)
  }
  
  structure(
    list(
      data_f = data_f,
      required_columns = required_columns,
      model_f = model_f,
      required_packages = required_packages,
      description = description,
      shiny_module = create_shiny_module_funcs(data_f = data_f,
                                               model_f = model_f,
                                               outputShiny = outputShiny,
                                               renderShiny = renderShiny)
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
                                      outputShiny,
                                      renderShiny){
  assert_that(
    is.function(data_f),
    is.function(model_f),
    is.function(outputShiny),
    is.function(renderShiny)
  )
  
  server_func <- function(input, output, session, view_id, ...){
    
    gadata <- shiny::reactive({
      
      view_id <- view_id()
      
      data_f(view_id, ...)
      
    })
    
    model_output <- shiny::reactive({
      shiny::validate(shiny::need(gadata(), message = "Waiting for data"))
      gadata <- gadata()
      
      model_f(gadata, ...)
      
    })
    
    output$ui_out <- renderShiny({
      shiny::validate(shiny::need(model_output(), message = "Waiting for model output"))
      
      message("Rendering model output")
      
      model_output()
      
    }, ...)
    
    return(model_output)
  }
  
  ui_func <- function(id, ...){
    ns <- shiny::NS(id)
    
    outputShiny(outputId = ns("ui_out"), ...)
    
  }
  
  list(
    ui = ui_func,
    server = server_func
  )
  
}


