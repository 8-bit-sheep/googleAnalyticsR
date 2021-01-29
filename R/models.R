#' Use a model function created by \link{ga_model_make}
#'
#' @param viewId The GA viewId to operate on
#' @param model A file location of a model object or a model object
#'   created by \link{ga_model_make}
#' @param load_libs Whether to load the library requirements into your namespace
#' @param ... Other arguments to pass into the model as needed
#'
#' @export
#' @family GA modelling functions
ga_model <- function(viewId, model, load_libs = TRUE, ...){
  
  # a filepath
  if(is.character(model)){
    model <- ga_model_load(model)
  }
  
  assert_that(is.ga_model(model))
  
  check_packages_installed(model$required_packages, load_them = load_libs)
  
  gadata <- model$data_f(viewId, ...)
  
  myMessage("Downloaded data: Rows:", 
            nrow(gadata), " Columns:", 
            paste(names(gadata), collapse = " "), level = 3)
  
  if(length(intersect(names(gadata), model$required_columns)) == 0){
    stop("All required columns were not found in returned data.
         Got: ", paste(names(gadata), collapse=" "),
         " Needed: ", paste(model$required_columns, collapse = " "),
         call. = FALSE)
  }
  
  o <- model$model_f(gadata, ...)
  myMessage("Applied model to data", level = 3)
  
  if(!is.null(model$output_f)){
    myMessage("Creating plot", level = 3)    
    p <- model$output_f(o, ...)
  } else {
    p <- NULL
  }
  
  structure(
    list(input = gadata, 
         output = o, 
         plot = p, 
         model = model, 
         viewId = viewId, 
         args = list(...)), 
    class = c("ga_model_result","list")
  )

}

is.ga_model_result <- function(x){
  inherits(x, "ga_model_result")
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
#' @examples 
#' 
#' # models used in ga_model_example() are here:
#' location <- system.file("models","decomp_ga.gamr", 
#'                         package = "googleAnalyticsR")
#' 
#' ga_model_load(location)
ga_model_load <- function(filename = "my-model.gamr"){
  
  assert_that(is.readable(filename))
  
  model <- readRDS(filename)
  
  assert_that(is.ga_model(model))
  myMessage("Loaded model from ", filename, level = 2)
  model
  
}

#' Load an example model
#' 
#' @param name name of the model - set to "list" to show available files
#' @export
#' @import assertthat
#' @family GA modelling functions
#' @examples 
#' 
#' # example .gamr files included with the package
#' ga_model_example()
#' 
#' # load one example
#' ga_model_example("ga4-trend.gamr")
ga_model_example <- function(name = "list"){
  
  if(name == "list"){
    return(list.files(system.file("models","examples", package = "googleAnalyticsR"), 
               include.dirs = FALSE))
  }
  
  # this will fetch from more places such as other packages and GCS eventually
  filename <- system.file("models","examples", name, package = "googleAnalyticsR")
  
  ga_model_load(filename)
}


#' Modelling function factory for Google Analytics data
#'
#' Create ga_model objects for easy application of models to data
#' 
#' @param data_f A function that gets the data 
#' @param required_columns What dimensions and metrics are required
#' @param model_f A function that inputs data, and outputs a list of assets -
#'   must take data from result of \code{data_f} in first argument
#' @param output_f A function that inputs the output from \code{model_f}, outputs a visualisation
#' @param required_packages The packages needed for \code{data_f} and \code{model_f} to work
#' @param description An optional description of what the model does
#' @param renderShiny A shiny render function that will create the output for \code{outputShiny} from \code{output_f}
#' @param outputShiny A shiny UI output function that will display the results \code{renderShiny}
#' @param inputShiny Optional input shiny functions (like \code{dateInput()}) that will be used within the model's Shiny module.  The id should be exactly the same as one of the variables in the model functions.
#'
#' @details 
#' 
#' The passed functions should all have \code{...} to make them flexible
#'   in what arguments can be added.  Do not have the same argument names in both functions.
#' The \code{data_f} function result will feed to \code{model_f}
#'
#' @export
#' @import assertthat
#' @family GA modelling functions
#' 
#' @return A \code{ga_model} object to pass to \link{ga_model}
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
#'    decompose(ts(df$sessions, frequency = 7))
#'  }
#'
#'  decomp_ga <- ga_model_make(get_model_data,
#'                             required_columns = c("date", "sessions"),
#'                             model_f = decompose_sessions,
#'                             description = "Performs decomposition and creates plot")
#'
#'  # fetches data and outputs decomposition
#'  ga_model(81416156, decomp_ga)
#'
#'  # save the model for later
#'  model_location <- "decomp_ga.gamr"
#'  ga_model_save(decomp_ga, filename = model_location)
#'
#'  # can load model from file
#'  ga_model(81416156, model_location)
#'
#'  # or load model to an object and use 
#'  model2 <- ga_model_load(model_location)
#'
#'  ga_model(81416156, model2)
#'  
#'  # for shiny include functions for the UI and server rendering
#'  decomp_ga <- ga_model_make(get_model_data,
#'                             required_columns = c("date", "sessions"),
#'                             model_f = decompose_sessions,
#'                             output_f = function(df, ...){plot(df)},
#'                             description = "Performs decomposition and creates a plot",
#'                             outputShiny = shiny::plotOutput,
#'                             renderShiny = shiny::renderPlot)
#'
#' }
ga_model_make <- function(
  data_f,
  required_columns,
  model_f,
  output_f = function(df, ...){plot(df)},
  required_packages = NULL,
  description = NULL,
  outputShiny = shiny::plotOutput,
  renderShiny = shiny::renderPlot,
  inputShiny = NULL){
  
  Model(
    data_f            = data_f,
    required_columns  = required_columns,
    model_f           = model_f,
    output_f          = output_f,
    required_packages = required_packages,
    description       = description,
    shiny_module = create_shiny_module_funcs(
      data_f = data_f,
      model_f = model_f,
      output_f = output_f,
      outputShiny = outputShiny,
      renderShiny = renderShiny,
      inputShiny = inputShiny)
  )
  
}

#' Edit a created ga_model
#' 
#' Change features of a model by changing the functions within it.
#' 
#' @param model The model to edit - if a filepath will load model and 
#'   save back edited model to the same file
#' @inheritParams ga_model_make
#' 
#' 
#' @export
#' @family GA modelling functions
ga_model_edit <- function(
  model,
  data_f = NULL,
  required_columns = NULL,
  model_f = NULL,
  required_packages = NULL,
  description = NULL,
  outputShiny = NULL,
  renderShiny = NULL,
  inputShiny = NULL,
  output_f = NULL){
  
  save_me <- ""
  if(is.character(model)){
    save_me <- model
    model <- ga_model_load(model)
  }
  
  assert_that(is.ga_model(model))
  myMessage("Editing model: ", model$description, level = 3)
  
  data_f2              <- model$data_f
  required_columns2    <- model$required_columns
  model_f2             <- model$model_f
  output_f2            <- model$output_f
  required_packages2   <- model$required_packages
  description2         <- model$description
  shiny_module_ui2     <- model$shiny_module$ui
  shiny_module_server2 <- model$shiny_module$server
  shiny_module_uiInput2 <- model$shiny_module$uiInput
  
  data_f2            <- assign_new(data_f, data_f2, is.function)
  required_columns2  <- assign_new(required_columns, required_columns2, is.character)
  model_f2           <- assign_new(model_f, model_f2, is.function)
  output_f2          <- assign_new(output_f, output_f2, is.function)
  required_packages2 <- assign_new(required_packages, required_packages2, is.character)
  description2       <- assign_new(description, description2, assertthat::is.string)

  # only do this bit if a shiny module is present
  if(any(!is.null(outputShiny), !is.null(renderShiny), !is.null(inputShiny))){

    shiny_module_ui2     <- assign_new(outputShiny, shiny_module_ui2)
    shiny_module_server2 <- assign_new(renderShiny, shiny_module_server2)
    shiny_module_uiInput2 <- assign_new(inputShiny, shiny_module_uiInput2, is.inputShiny)
  }
  
  shiny_module <- create_shiny_module_funcs(
    data_f = data_f2,
    model_f = model_f2,
    output_f = output_f2,
    outputShiny = shiny_module_ui2,
    renderShiny = shiny_module_server2,
    inputShiny = shiny_module_uiInput2)
  
  
  model <- Model(
      data_f            = data_f2,
      required_columns  = required_columns2,
      model_f           = model_f2,
      required_packages = required_packages2,
      description       = description2,
      output_f          = output_f2,
      shiny_module      = shiny_module
    )
  
  if(nzchar(save_me)) ga_model_save(model, filename = save_me)
  
  model
}

is.inputShiny <- function(x){
  inherits(x, "shiny.tag") || inherits(x, "shiny.tag.list")
}

#' @noRd
Model <- function(
  data_f,
  required_columns  = NULL,
  model_f           = NULL,
  required_packages = NULL,
  description       = NULL,
  output_f          = NULL,
  shiny_module      = NULL){
  
  assert_that(
    is.function(data_f),
    is.character(required_columns),
    is.function(model_f)
  )
  
  if(!any(function_args(data_f, TRUE) == "...")){
    stop("data_f() arguments need to include ...", call.=FALSE)
  }
  
  if(!any(function_args(model_f, TRUE) == "...")){
    stop("model_f() arguments need to include ...", call.=FALSE)
  }
  
  if(!is.null(output_f) && !any(function_args(output_f, TRUE) == "...")){
    stop("output_f() arguments need to include ...", call.=FALSE)
  }
  
  # using id breaks shiny modules
  if("id" %in% c(function_args(data_f), function_args(model_f), function_args(output_f))){
    stop("Don't use 'id' in function arguments - it clashes with Shiny module's arg: id", call.=FALSE)
  }
  
  # data_f must include view_id as first argument
  if(function_args(data_f)[[1]] != "view_id"){
    stop("Must have 'view_id' as first argument of data_f", call.=FALSE)
  }
  
  structure(
    list(
      data_f            = data_f,
      required_columns  = required_columns,
      model_f           = model_f,
      required_packages = required_packages,
      description       = description,
      output_f          = output_f,
      shiny_module      = shiny_module
    ), 
    class = "ga_model"
  )
}


is.ga_model <- function(x){
  inherits(x, "ga_model")
}

is.ga_model_list <- function(x){
  all(lapply(x, is.ga_model))
}

is.shinyTagList <- function(x){
  inherits(x, "shiny.tag.list")
}

extract_ids <- function(inputShiny){
  if(is.shinyTagList(inputShiny)){
    input_ids <- lapply(inputShiny, extract_from_list, id_regex="id$")
    if(any(lapply(input_ids, length) > 1)){
      stop("inputShiny must be only one level deep of nesting e.g. tagList(xxxInput(), yyyInput()), not tagList(tagList(xxxInput(), yyyInput()), zzzInput())", call. = FALSE)
    }
  } else {
    input_ids <- list(extract_from_list(inputShiny, id_regex = "id$"))
  }
  input_ids
}

replace_ids <- function(inputShiny, ids, ns){
  f <- function(x, ids, ns){
    replace_in_list(x, ids, ns(ids))
  }
  if(is.shinyTagList(inputShiny)){
    inputs <- mapply(f, inputShiny, ids, 
                     MoreArgs = list(ns = ns),
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else {
    inputs <- f(inputShiny, ids[[1]], ns)
  }
  inputs
}

create_shiny_module_ui <- function(outputShiny,inputShiny, input_ids){
  assert_that_ifnn(inputShiny, is.inputShiny)
  assert_that(is.function(outputShiny))
  
  function(id, ...){
    ns <- shiny::NS(id)
    if(!is.null(input_ids)){
      inputShiny <- replace_ids(inputShiny, input_ids, ns)
    } 
    shiny::tagList(
      inputShiny,
      outputShiny(outputId = ns("ui_out"), ...)
    )
  }
}


#' @noRd
#' @import assertthat
#' @importFrom utils modifyList
create_shiny_module_funcs <- function(data_f,
                                      model_f,
                                      output_f,
                                      outputShiny,
                                      renderShiny,
                                      inputShiny = NULL
                                      ){
  myMessage("Creating Shiny modules", level = 2)
  if(any(is.null(output_f), is.null(outputShiny), is.null(renderShiny))){
    myMessage("Can't create Shiny module as necessary functions are NULL", 
              level = 3)
    return(NULL)
  }
  
  assert_that(
    is.function(data_f),
    is.function(model_f),
    is.function(outputShiny),
    is.function(renderShiny),
    is.function(output_f)
  )
  
  input_ids <- extract_ids(inputShiny)
  
  ui <- create_shiny_module_ui(outputShiny, inputShiny, input_ids)
  
  server <- function(id, view_id, ...){
  
    shiny::moduleServer(
      id,
      function(input, output, session){
        dots <- list(...)

        myMessage("shiny args passed: ", 
                  paste(names(dots),"=", dots, collapse = ","),
                  level = 2)
        
        copy_input_ids <- function(input_ids, input, dots){
          new <- lapply(input_ids, function(x){dots[[x]] <- input[[x]]})
          modifyList(dots, setNames(new, input_ids))
        }

        gadata <- shiny::reactive({
          shiny::req(view_id())

          dots <- copy_input_ids(input_ids, input, dots)
          
          myMessage("Fetching data for view_id:",view_id(), level = 3)
          execute_model_function(data_f,
                                 dependency = list(view_id = view_id()),
                                 dots = dots)
          
        })
        
        model_output <- shiny::reactive({
          shiny::validate(shiny::need(gadata(), 
                                      message = "Waiting for data"))
          dots <- copy_input_ids(input_ids, input, dots)
          myMessage("Modelling data", level = 3)
          execute_model_function(model_f,
                                 dependency = list(gadata()),
                                 dots = dots)
          
        })
        
        output$ui_out <- renderShiny({
          shiny::validate(shiny::need(model_output(), 
                                      message = "Waiting for model output"))
          dots <- copy_input_ids(input_ids, input, dots)
          myMessage("Rendering model output", level = 3)
          execute_model_function(output_f,
                                 dependency = list(model_output()),
                                 dots = dots)
          
        })
        
        return(model_output)
      })
  }
  
  list(
    ui = ui,
    server = server,
    uiInput = inputShiny
  )
  
}

execute_model_function <- function(f, dependency, dots){
  assert_that(is.function(f), is.list(dependency))
  
  myMessage("function_vars: ", paste(function_args(f), collapse = ", "))
  
  tryCatch(do.call(f, args = c(dependency, eval_input_list(dots))),
           error = function(err){
             stop("Problem executing model function - ", err$message, 
                  call. = FALSE)
           })
}

# force evaluation to reactive inputs update
eval_input_list <- function(dots){
  lapply(dots, function(x){
    if(inherits(x, "reactive")){
      do.call(x, args = list())
    } else {
      x
    }
  })
}


#' Write the ga_model functions to a file
#' 
#' @param model The \code{ga_model} object to extract functions from to write, or a filepath to a model
#' @param filepath The filepath to write the functions to
#' 
#' @export
#' @family GA modelling functions
#' @import assertthat
ga_model_write <- function(model, filepath = "ga_model.R"){
  
  if(is.character(model)){
    model <- ga_model_load(model)
  }
  
  assert_that(is.ga_model(model))
  
  if(!is.null(model$required_packages)){
    libs <- paste0("library(",model$required_packages,")")
  } else {
    libs <- "# no libraries needed"
  }
  
  the_text <- 
    c(sprintf("# ga_model: %s\n", model$description),
      libs,
      write_f("\n# fetch data\ndata_f <-", model$data_f),
      write_f("\n# model data\nmodel_f <-", model$model_f),
      write_f("\n# output data\noutput_f <-", model$output_f),
      write_f("\n# shiny input function\nuiInput <-", model$shiny_module$uiInput),
      "\n# use via ga_model_make()")
  
  writeLines(the_text, con = filepath)
  suppressMessages(formatR::tidy_file(filepath, width.cutoff = 80))
  myMessage("Written model to ", filepath, level = 3)
  
  model
}

write_f <- function(name, f){
  if(is.null(f)) return("function(){NULL}")
  c(name, deparse(f))
}

#' Refresh a model
#' 
#' Sometimes necessary if functions were created under differing package versions
#' 
#' @param model Model or file location of model .gamr file
#' 
#' @export
ga_model_refresh <- function(model){
  save_me <- ""
  if(is.character(model)){
    save_me <- model
    model <- ga_model_load(model)
  }
  
  new_model <- ga_model_edit(model,
    data_f = model$data_f,
    required_columns = model$required_columns,
    model_f = model$model_f,
    required_packages = model$required_packages,
    description = model$description,
    outputShiny = model$outputShiny,
    renderShiny = model$renderShiny,
    inputShiny = model$inputShiny,
    output_f = model$output_f
  )
  
  if(nzchar(save_me)){
    ga_model_save(new_model, filename = save_me)
  }
  
  new_model
  
  
}

