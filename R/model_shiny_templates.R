#' Get a Shiny template file
#' 
#' Gets a pre-created template from the googleAnalyticsR samples
#' 
#' @param name the template name
#' @param read_lines If TRUE will use \code{readLines()} to print out the template contents
#' 
#' @export
#' @family GA modelling functions
ga_model_shiny_template <- function(name = "list", read_lines = FALSE){
  
  if(name == "list"){
    return(list.files(system.file("models","shiny", 
                                  package = "googleAnalyticsR")))
  }
  
  f <- system.file("models","shiny",name, 
                   package = "googleAnalyticsR")
  if(!nzchar(f)){
    stop("Couldn't find template named ", name, call. = FALSE)
  }
  
  if(read_lines){
    return(readLines(f))
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
#' @param date_range Most templates support a {{ date_range }} global input for the data import functions, set this to FALSE to remove it
#' @param ui_f A function to generate the UI from the models list argument - default is \link{ga_model_shiny_ui}
#' @param template_type Shiny app templates can hold only the ui of an app, which means the server and/or the header section will be loaded via the standard boilerplate from \code{"server_boilerplate.R"} and/or \code{"header_boilerplate.R"}. Select \code{"full"} if the template holds everything including header, ui and server code
#' @param ... Extra macro variables the template may support: a named list with the name being a template variable
#' 
#' @details 
#' 
#' As \link{ga_model} objects have standardised code, they can be used to build standard templated Shiny apps.  Templates are made using the \link[whisker]{whisker.render} function
#' 
#' Templates hold macro variables indicated via \code{ \{\{ macro_name \}\} } in the Shiny app template code. See \code{ga_model_shiny_template("template_full.R", TRUE)} for an example showing a minimal viable app.
#' 
#' Template contain code to allow multi-user login via Google OAuth2.
#' 
#' As the header and server parts of the Shiny code are generic, they can also be templated leaving you only needing to create the UI part of the Shiny code.  This is the default of \code{template_type = "ui-only"}.  Use the other template_type's if your Shiny template does include header or server code.
#' 
#' Some templates are included with the package, seen via \code{ga_model_shiny_template("list")}
#' 
#' @section Template macro variables:
#' 
#' \itemize{
#'  \item{\code{\{\{\{ model_libraries \}\}\}}}{- Adds \code{library()} calls based on models$required_packages}
#'  \item{\code{\{\{\{ web_json \}\}\}}}{- Adds Google OAuth2 client for web applications}
#'  \item{\code{\{\{\{ scopes \}\}\}}}{- Adds Google Oauth2 scopes for the API calls}
#'  \item{\code{\{\{\{ deployed_url \}\}\}}}{- Adds \code{option(googleAuthR.redirect)} option for deployed Shiny apps}
#'  \item{\code{\{\{\{ model_load \}\}\}}}{- Adds \link{ga_model_load} calls loading all models in the list passed to this function's \code{models} argument}
#'  \item{\code{\{\{\{ shiny_title \}\}\}}}{- Adds the title to the Shiny app}
#'  \item{\code{\{\{\{ auth_ui \}\}\}}}{- Adds the correct dropdown Shiny module for picking a GA4 or Universal Analytics properties}
#'  \item{\code{\{\{\{ date_range \}\}\}}}{- Adds a \code{shiny::dateInput()} date selector with id "date_range" for use in model's data fetching functions}
#'  \item{\code{\{\{\{ model_ui \}\}\}}}{- Adds the models UI elements as configured in the \link{ga_model} object}
#'  \item{\code{\{\{\{ auth_server \}\}\}}}{- Adds the authentication module's server side function}
#'  \item{\code{\{\{\{ auth_accounts \}\}\}}}{- Adds a call to \link{ga_account_list} for the appropriate GA account type (GA4 or Universal)}
#'  \item{\code{\{\{\{ model_server \}\}\}}}{- Adds the server side module for the models as configured in the \link{ga_model} configuration}
#'  \item{\code{\{\{\{ your_argument \}\}\}}}{- You can pass in your own custom variables to the template via the \code{...} argument of this function if they are named the same as the template macro variable}
#' }
#' 
#' @export
#' @importFrom assertthat is.readable is.writeable
#' @importFrom whisker whisker.render
#' 
#' @examples
#' 
#' # see Shiny templates included with the package
#' ga_model_shiny_template("list")
#' 
#' \dontrun{
#' 
#' # a universal analytics model using default template "template1.R"
#' ga_model_shiny(
#'   ga_model_example("decomp_ga.gamr"), auth_dropdown = "universal")
#' 
#' # multiple models
#' m3 <- ga_model_example("time-normalised.gamr")
#' m4 <- ga_model_example("ga-effect.gamr")
#' 
#' # launch in gentelella template
#' ga_model_shiny(list(m4, m3), auth_dropdown = "universal",
#'               template = ga_model_shiny_template("gentelella.R"))
#' 
#' # custom shinydashboard template              
#' ## make a function to output the custom shinydashboard tabs
#' shinydashboard_ui_menu <- function(models){
#'   model_n <- paste0("model", seq_along(models)) 
#'   labels <- lapply(models, function(x) substr(x$description, 0,14))
#'   
#'   f <- function(model_n, label){
#'   paste(sprintf("menuItem('%s', tabName = '%s')", 
#'         label, model_n), collapse = ",\n")}
#'  
#'   mapply(f, model_n, labels, SIMPLIFY = FALSE, USE.NAMES = FALSE)
#' }
#' 
#' ## supply custom function for wrapping the model_ui output with tabItem()
#' shinydashboard_ui <- function(model_n){
#'    paste(sprintf("tabItem(tabName = '%s',%s$ui('%s'))", 
#'          model_n, model_n, model_n),collapse = ",\n")}
#'          
#' m3 <- ga_model_example("time-normalised.gamr")
#' m4 <- ga_model_example("ga-effect.gamr")
#' models <- list(m3, m4)
#' 
#' ## launch shiny app with the models in each tab
#' ## model_tabs is via ... and a custom macro in the shinydashboard template
#' ga_model_shiny(models, auth_dropdown = "universal", 
#'      template = ga_model_shiny_template("shinydashboard.R"), 
#'      ui_f = shinydashboard_ui, 
#'      model_tabs = shinydashboard_ui_menu(models))
#' }
#' @family GA modelling functions
ga_model_shiny <- function(
  models,
  template = ga_model_shiny_template("template1.R"),
  template_type = c("ui-only","ui-header","full"),
  title = "ga_model_shiny",
  auth_dropdown = c("ga4","universal","none"),
  web_json = Sys.getenv("GAR_CLIENT_WEB_JSON"),
  date_range = TRUE,
  scopes = "https://www.googleapis.com/auth/analytics.readonly",
  deployed_url = "",
  local_file = "",
  ui_f = ga_model_shiny_ui,
  ...){
  
  auth_dropdown <- match.arg(auth_dropdown)
  template_type <- match.arg(template_type)
  
  if(is.ga_model(models)){
    models <- list(models)
  }
  
  model_locations <- lapply(models, model_path)
  # model1, model2, etc.
  names(model_locations) <- paste0("model", seq_along(models))
  
  assert_that(is.readable(template),
              nzchar(web_json),
              nzchar(scopes))
  
  model_template <- make_model_template(
    model_locations, 
    date_range = date_range, 
    ga_model_shiny_ui_f = ui_f)
  
  txt <- readLines(template)
  values <- c(list(...),
              make_date_range(date_range),
              make_auth_dropdown(auth_dropdown), 
              model_locations,
              model_template,
              make_model_libraries(models),
              web_json = web_json,
              scopes = scopes,
              shiny_title = title)
  myMessage("passed template values:\n", 
            paste(names(values),"=",values, collapse = "\n"),
            level = 3)
  
  render <- whisker.render(txt, values)
  
  if(template_type == "ui-only"){
    myMessage(
      "Adding ga_model_shiny_template('header_boilerplate.R') to Shiny code",
      level = 3)
    
    # add the header boiler plate
    hdr_txt <- ga_model_shiny_template("header_boilerplate.R", 
                                       read_lines = TRUE)
    hdr_render <- whisker.render(hdr_txt, values)
    render <- c(hdr_render,
                render)
  }
  
  if(template_type %in% c("ui-only","ui-header")){
    myMessage(
      "Adding ga_model_shiny_template('server_boilerplate.R') to Shiny code",
      level = 3)
    
    # add the server boiler plate
    server_txt <- ga_model_shiny_template("server_boilerplate.R", 
                                          read_lines = TRUE)
    server_render <- whisker.render(server_txt, values)
    render <- c(render,
                server_render)
  }
  
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

make_date_range <- function(date_range){
  if(!date_range) return("br()")
  
  list(date_range = 'dateRangeInput("date_range", 
                        "Date Range", 
                        start = Sys.Date() - 400,
                        end = Sys.Date() - 1)')
}

make_model_libraries <- function(models){
  the_libs <- unique(unlist(lapply(models, function(x) x$required_packages)))
  list(
    model_libraries = paste(sprintf("library(%s)", the_libs), collapse = "\n")
  )
}

make_model_template <- function(model_locations, 
                                date_range,
                                ga_model_shiny_ui_f){
  
  # add dependency on global input$date_range?
  if(!date_range){
    model_server <- paste(
      sprintf("%s$server('%s', view_id = view_id)", 
              names(model_locations), names(model_locations)), 
      collapse = "\n")
  } else {
    model_server <- paste(
      sprintf("%s$server('%s', view_id = view_id, 
                         date_range = reactive(input$date_range))", 
              names(model_locations), names(model_locations)), 
      collapse = "\n")
  }
  
  list(
    model_load = paste(
      sprintf("%s <- ga_model_shiny_load('%s')", 
              names(model_locations), model_locations), 
      collapse = "\n"),
    model_ui = ga_model_shiny_ui_f(names(model_locations)),
    model_server = model_server
  )
  
}

#' How to generate the model objects HTML
#' 
#' This is the default way to create model UIs generated from the model name e.g. 'model1', 'model2' etc.
#' 
#' @param model_n The id of the model within Shiny.  This is usually \code{'modelN'} when N is the index of the model in the list
#' 
#' @details 
#' 
#' \code{ga_model_shiny_ui()} generates the UI for loading the model.  At minimum it needs to load \code{model1$ui('model1')} which is the default.  You may want to add some logic to make menu items for some templates, which this function helps facilitate.
#' 
#' @export
#' @examples 
#' 
#' ga_model_shiny_ui("model1")
#' @rdname ga_model_shiny_load
ga_model_shiny_ui <- function(model_n){
  paste(
    sprintf("%s$ui('%s')", model_n, model_n), 
    collapse = ",\n")
}

shinydashboard_ui <- function(model_n){
  
  paste(
    sprintf(
      "tabItem(tabName = '%s',
         %s$ui('%s'))", 
      model_n, model_n, model_n
    ),
    collapse = ",\n"
  )
}

shinydashboard_ui_menu <- function(models){
  
  model_n <- paste0("model", seq_along(models)) 
  labels <- lapply(models, function(x) substr(x$description, 0,14))
  
  f <- function(model_n, label){
    paste(
      sprintf(
        "menuItem('%s', tabName = '%s')", 
        label, model_n
      ),
      collapse = ",\n"
    )}
  
  mapply(f, model_n, labels, SIMPLIFY = FALSE, USE.NAMES = FALSE)
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
#' @family GA modelling functions
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

