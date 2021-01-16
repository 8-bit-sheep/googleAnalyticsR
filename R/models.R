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
ga_model_example <- function(name = "list"){
  
  if(name == "list"){
    return(list.files(system.file("models", package = "googleAnalyticsR"), 
               include.dirs = FALSE))
  }
  
  # this will fetch from more places such as other packages and GCS eventually
  filename <- system.file("models", name, package = "googleAnalyticsR")
  
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

#' Get a Shiny template file
#' 
#' Gets a pre-created template from the googleAnalyticsR samples
#' 
#' @param name the template name
#' 
#' @export
ga_model_shiny_template <- function(name = "list"){
  
  if(name == "list"){
    return(list.files(system.file("models","shiny", 
                       package = "googleAnalyticsR")))
  }
  
  f <- system.file("models","shiny",name, 
              package = "googleAnalyticsR")
  if(!nzchar(f)){
    stop("Couldn't find template named", name, call. = FALSE)
  }
  
  f
}


#' Create a Shiny app from a ga_model file
#' 
#' @param models The \link{ga_model} file location ("my_model.gamr") or a \link{ga_model} object - can pass in multiple as a list
#' @param template The template file for the Shiny app
#' @param auth_dropdown What type of account picker to include
#' @param web_json The client.id json file for Web
#' @param scopes The scope the API requests will be under
#' @param title The title of the Shiny app
#' @param local_file If not empty, will not launch Shiny app but write code to the file location you put here
#' @param deployed_url If deploying Shiny app to a server, put the URL of the deployed app here so the authentication will redirect to the correct place
#' @param ... Extra variables the template may support
#' 
#' @export
#' @importFrom assertthat is.readable
#' @importFrom whisker whisker.render
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ga_model_shiny(
#'   ga_model_example("decomp_ga.gamr"), 
#'   template = ga_model_shiny_template("template_ua.R"))
#' 
#' }
ga_model_shiny <- function(
  models,
  template = ga_model_shiny_template("template1"),
  title = "ga_model_shiny",
  auth_dropdown = c("ga4","universal","none"),
  web_json = Sys.getenv("GAR_CLIENT_WEB_JSON"),
  scopes = "https://www.googleapis.com/auth/analytics.readonly",
  deployed_url = "",
  local_file = "",
  ...){
  
  auth_dropdown <- match.arg(auth_dropdown)
  if(is.ga_model(models)){
    models <- list(models)
  }
  
  model_locations <- lapply(models, model_path)
  # model1, model2, etc.
  names(model_locations) <- paste0("model", seq_along(models))
  
  assert_that(is.readable(template),
              nzchar(web_json),
              nzchar(scopes))
  
  txt <- readLines(template)
  values <- c(list(...),
              make_auth_dropdown(auth_dropdown), 
              model_locations,
              make_model_template(model_locations),
              make_model_libraries(models),
              web_json = web_json,
              scopes = scopes,
              shiny_title = title)
  myMessage("passed template values:\n", 
            paste(names(values),"=",values, collapse = "\n"),
            level = 3)

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

make_model_libraries <- function(models){
  the_libs <- unique(unlist(lapply(models, function(x) x$required_packages)))
  list(
    model_libraries = paste(sprintf("library(%s)", the_libs), collapse = "\n")
  )
}

make_model_template <- function(model_locations){
  list(
    model_load = paste(
      sprintf("%s <- ga_model_shiny_load('%s')", 
              names(model_locations), model_locations), 
      collapse = "\n"),
    model_ui = paste(
      sprintf("%s$ui('%s')", 
              names(model_locations), names(model_locations)), 
      collapse = ",\n"),
    model_server = paste(
      sprintf("%s$server('%s', view_id = view_id)", 
              names(model_locations), names(model_locations)), 
      collapse = "\n")
  )
}

make_auth_dropdown <- function(type){
  switch(type,
    none = NULL,
    universal = list(
      auth_ui = "authDropdownUI('auth_menu', inColumns = TRUE)",
      auth_server = "callModule(authDropdown, 'auth_menu', ga.table = al)",
      auth_accounts = "al <- reactive({req(token);ga_account_list()})"),
    ga4 = list(
      auth_ui = "accountPickerUI('auth_menu', inColumns = TRUE)",
      auth_server = "accountPicker('auth_menu', ga_table = al, id_only = TRUE)",
      auth_accounts = "al <- reactive({req(token);ga_account_list('ga4')})"
    ))
}

#' Load one model into a Shiny template
#' 
#' @param model_n The templated name of a model e.g. 'model1'
#' @param ... Other arguments passed from shiny server
#' @export
ga_model_shiny_load <- function(model_n, ...){
  model <- tryCatch(
    ga_model_load(model_n),
    error = function(err){
      NULL
    })
  if(is.null(model)){
    modelUi <- function(...) NULL
    modelServer <- function(...) NULL
  } else {
    modelUi <- model$shiny_module$ui
    modelServer <- model$shiny_module$server
  }
    
  list(
    ui = modelUi,
    server = modelServer,
    model = model
  )
}

model_path <- function(m){
  if(is.ga_model(m)){
    tmp_model <- tempfile(fileext = ".gamr")
    ga_model_save(m, filename = tmp_model)
    model_location <- tmp_model
  } else {
    model_location <- m
  }
  
  assert_that(is.readable(model_location))
  
  normalizePath(model_location)
}

