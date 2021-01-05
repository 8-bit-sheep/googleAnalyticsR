#' Use a model function created by ga_model_make
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
ga_model_load <- function(filename = "my-model.gamr"){
  
  assert_that(is.readable(filename))
  
  model <- readRDS(filename)
  
  assert_that(is.ga_model(model))
  myMessage("Loaded model from ", filename, level = 3)
  model
  
}

#' Load an example model
#' 
#' @param name name of the model
#' @param location location of model
#' @export
#' @import assertthat
#' @family GA modelling functions
ga_model_example <- function(name, location = "googleAnalyticsR"){
  # this will fetch from more places such as other packages and GCS eventually
  filename <- system.file("models", name, package = location)
  
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
#'  model_location <- "inst/models/decomp_ga.gamr"
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
#'                             output_f = graphics::plot,
#'                             description = "Performs decomposition and creates a plot",
#'                             outputShiny = shiny::plotOutput,
#'                             renderShiny = shiny::renderPlot)
#'
#' }
ga_model_make <- function(data_f,
                          required_columns,
                          model_f,
                          output_f = graphics::plot,
                          required_packages = NULL,
                          description = NULL,
                          outputShiny = shiny::plotOutput,
                          renderShiny = shiny::renderPlot){
  
  Model(
      data_f            = data_f,
      required_columns  = required_columns,
      model_f           = model_f,
      output_f          = output_f,
      required_packages = required_packages,
      description       = description,
      shiny_module = create_shiny_module_funcs(data_f = data_f,
                                               model_f = model_f,
                                               output_f = output_f,
                                               outputShiny = outputShiny,
                                               renderShiny = renderShiny)
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
ga_model_edit <- function(model,
                          data_f = NULL,
                          required_columns = NULL,
                          model_f = NULL,
                          required_packages = NULL,
                          description = NULL,
                          outputShiny = NULL,
                          renderShiny = NULL,
                          output_f = NULL){
  
  save_me <- ""
  if(is.character(model)){
    save_me <- model
    model <- ga_model_load(model)
  }
  
  assert_that(is.ga_model(model))
  
  data_f2              <- model$data_f
  required_columns2    <- model$required_columns
  model_f2             <- model$model_f
  output_f2            <- model$output_f
  required_packages2   <- model$required_packages
  description2         <- model$description
  shiny_module_ui2     <- model$shiny_module$ui
  shiny_module_server2 <- model$shiny_module$server
  
  data_f2            <- assign_new(data_f, data_f2, is.function)
  required_columns2  <- assign_new(required_columns, required_columns2, is.character)
  model_f2           <- assign_new(model_f, model_f2, is.function)
  output_f2          <- assign_new(output_f, output_f2, is.function)
  required_packages2 <- assign_new(required_packages, required_packages2, is.character)
  description2       <- assign_new(description, description2, assertthat::is.string)
  
  if(any(!is.null(outputShiny), !is.null(renderShiny))){
    assert_that(
      !is.null(outputShiny),
      !is.null(renderShiny)
    )
    shiny_module_ui2     <- assign_new(outputShiny, shiny_module_ui2)
    shiny_module_server2 <- assign_new(renderShiny, shiny_module_server2)
  }
  
  shiny_module <- create_shiny_module_funcs(data_f = data_f2,
                                            model_f = model_f2,
                                            output_f = output_f2,
                                            outputShiny = shiny_module_ui2,
                                            renderShiny = shiny_module_server2)


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

#' @noRd
Model <- function(data_f,
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



#' @noRd
#' @import assertthat
create_shiny_module_funcs <- function(data_f,
                                      model_f,
                                      output_f,
                                      outputShiny,
                                      renderShiny
                                      ){
  
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
  
  ui <- function(id, ...){
    ns <- shiny::NS(id)
    
    outputShiny(outputId = ns("ui_out"), ...)
    
  }
  
  server <- function(id, view_id, ...){
  
    shiny::moduleServer(
      id,
      function(input, output, session){
        dots <- list(...)
        myMessage("shiny args passed: ", 
                  paste(names(dots),"=", dots, collapse = ","),
                  level = 2)
        gadata <- shiny::reactive({
          req(view_id())
          
          myMessage("Fetching data for view_id:",view_id(), level = 3)
          execute_model_function(data_f,
                                 dependency = list(view_id = view_id()),
                                 dots = dots)
          
        })
        
        model_output <- shiny::reactive({
          shiny::validate(shiny::need(gadata(), 
                                      message = "Waiting for data"))
          
          myMessage("Modelling data", level = 3)
          execute_model_function(model_f,
                                 dependency = list(gadata()),
                                 dots = dots)
          
        })
        
        output$ui_out <- renderShiny({
          shiny::validate(shiny::need(model_output(), 
                                      message = "Waiting for model output"))
          
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
    server = server
  )
  
}

execute_model_function <- function(f, dependency, dots){
  assert_that(is.function(f), is.list(dependency))
  tryCatch(do.call(f, args = c(dependency, eval_input_list(dots))),
           error = function(err){
             stop("Problem executing model function - ", err$message, call. = FALSE)
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
  
  the_text <- 
    c(sprintf("# ga_model: %s\n", model$description),
      paste0("library(",model$required_packages,")"),
      write_f("\n# fetch data\ndata_f", model$data_f),
      write_f("\n# model data\nmodel_f", model$model_f),
      write_f("\n# output data\noutput_f\n", model$output_f))
  
  writeLines(the_text, con = filepath)
  suppressMessages(formatR::tidy_file(filepath, width.cutoff = 80))
  myMessage("Written model to ", filepath, level = 3)
  
  model
}

write_f <- function(name, f){
  c(sprintf("%s <- function(%s)", name, paste(names(formals(f)), collapse = ",\n")),
    sprintf("%s\n", as.character(body(f))),
    "}")
}

#' Get a Shiny template file
#' 
#' Gets a pre-created template from the googleAnalyticsR samples
#' 
#' @param name the template name
#' 
#' @export
ga_model_shiny_template <- function(name){
  f <- system.file("models","shiny",paste0(name,".R"), 
              package = "googleAnalyticsR")
  if(!nzchar(f)){
    stop("Couldn't find template named", name, call. = FALSE)
  }
  
  f
}


#' Create a Shiny app from a ga_model file
#' 
#' @param model The \link{ga_model} file location ("my_model.gamr") or a \link{ga_model} object
#' @param template The template file for the Shiny app
#' @param web_json The client.id json file for Web
#' @param scopes The scope the API requests will be under
#' @param title The title of the Shiny app
#' @param local_file If not empty, will not launch Shiny app but write code to the file location you put here
#' @param ... Extra variables the template may support
#' 
#' @export
#' @importFrom assertthat is.readable
#' @importFrom whisker whisker.render
ga_model_shiny <- function(model,
                           template = ga_model_shiny_template("template1"),
                           title = "Generated be googleAnalyticsR::ga_model_shiny",
                           web_json = Sys.getenv("GAR_CLIENT_WEB_JSON"),
                           scopes = "https://www.googleapis.com/auth/analytics.readonly",
                           local_file = "",
                           ...){
  
  if(is.ga_model(model)){
    tmp_model <- tempfile(fileext = ".gamr")
    ga_model_save(model, filename = tmp_model)
    model_location <- tmp_model
  } else {
    assert_that(is.readable(model_location))
  }
  
  assert_that(is.readable(template),
              nzchar(web_json),
              nzchar(scopes))
  
  # get absolute file path
  model_location <- normalizePath(model_location)
  
  txt <- readLines(template)
  values <- c(list(...),
              web_json = web_json,
              scopes = scopes,
              ga_model = model_location,
              shiny_title = title,
              ga_model_name = "model1")

  
  render <- whisker.render(txt, values)
  
  if(nzchar(local_file)){
    writeLines(render, local_file)
    return(render)
  }
  
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp))
  
  writeLines(render, tmp)
  myMessage("Launching Shiny app from ", tmp, level = 3)
  shiny::runApp(tmp)
}

# this seems to be needed for Shiny mods?
ga_model_refresh <- function(model){
  ga_model_make(
    data_f = model$data_f,
    required_columns = model$required_columns,
    model_f = model$model_f,
    output_f = model$output_f,
    required_packages = model$required_packages,
    description = model$description,
    outputShiny = model$outputShiny,
    renderShiny = model$renderShiny
  )
}
