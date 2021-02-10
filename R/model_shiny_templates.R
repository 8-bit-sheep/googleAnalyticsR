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
    return(list.files(system.file("models","shiny_templates", 
                                  package = "googleAnalyticsR")))
  }
  
  f <- system.file("models","shiny_templates",name, 
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
#' @param template The template Shiny files for the Shiny app - passed to \code{shiny::runApp()}
#' @param header_boilerplate Whether to add header boilerplate to the template
#' @param auth_dropdown What type of account picker to include
#' @param web_json The client.id json file for Web
#' @param scopes The scope the API requests will be under
#' @param title The title of the Shiny app
#' @param local_folder If not empty, will not launch Shiny app but write code to the folder location you put here
#' @param deployed_url If deploying Shiny app to a server, put the URL of the deployed app here so the authentication will redirect to the correct place
#' @param date_range Most templates support a {{ date_range }} global input for the data import functions, set this to FALSE to remove it
#' @param ... Extra macro variables the template may support: a named list with the name being a template variable
#' 
#' @details 
#' 
#' As \link{ga_model} objects have standardised code, they can be used to build standard templated Shiny apps.  Templates are made using the \link[whisker]{whisker.render} function
#' 
#' Some templates are included with the package, seen via \code{ga_model_shiny_template("list")}
#' 
#' Templates hold macro variables indicated via \code{ \{\{ macro_name \}\} } in the Shiny app template code. See \code{ga_model_shiny_template("basic_app", TRUE)} for an example showing a minimal viable app.  Templates can be files such as ui.R or app.R files; folders containing ui.R, app.R files; or ui.R with html files for advanced themes - see \href{https://shiny.rstudio.com/articles/templates.html}{Shiny HTML templates}. All additional files that may be in the folder are also copied over (such as global.R or www/ folders)
#' 
#' Templates contain code to allow multi-user login via Google OAuth2.
#' 
#' If your template is pointing at a file such as ui.R or app.R it will create an app.R Shiny object.  If your template is pointing at a directory it will check for the presence of ui.R within the folder.  In either case if the server.R is missing it will use the boilerplate version from \code{ga_model_shiny_template("boilerplate")}
#' 
#' By default the Shiny app is launched which in most cases will prompt authorisation for your Google Analytics.  You can instead write the app out using \code{local_folder} to a valid location for deployment later.
#' 
#' @section Template macro variables:
#' 
#' \itemize{
#'  \item{\code{\{\{\{ model_libraries \}\}\}}}{- Adds \code{library()} calls based on models$required_packages}
#'  \item{\code{\{\{\{ web_json \}\}\}}}{- Adds Google OAuth2 client for web applications}
#'  \item{\code{\{\{\{ scopes \}\}\}}}{- Adds Google Oauth2 scopes for the API calls}
#'  \item{\code{\{\{\{ deployed_url \}\}\}}}{- Adds \code{option(googleAuthR.redirect)} option for deployed Shiny apps}
#'  \item{\code{\{\{\{ model_load \}\}\}}}{- Adds \link{ga_model_load} calls loading all models in the list passed to this function's \code{models} argument.  It creates R objects called 'model1', 'model2' etc. in the Shiny app code}
#'  \item{\code{\{\{\{ model_list \}\}\}}}{- Adds a list of the model objects after model_load.  Useful for creating custom functions in themes that can loop over model objects}
#'  \item{\code{\{\{\{ shiny_title \}\}\}}}{- Adds the title to the Shiny app}
#'  \item{\code{\{\{\{ auth_ui \}\}\}}}{- Adds the correct dropdown Shiny module for picking a GA4 or Universal Analytics properties}
#'  \item{\code{\{\{\{ date_range \}\}\}}}{- Adds a \code{shiny::dateInput()} date selector with id "date_range" for use in model's data fetching functions}
#'  \item{\code{\{\{\{ model_ui \}\}\}}}{- Adds the models UI elements as configured in the \link{ga_model} object.  It uses the object loaded above via the model_load macro.  It looks like \code{model1$ui('model1')} in the code.}
#'  \item{\code{\{\{\{ auth_server \}\}\}}}{- Adds the authentication module's server side function}
#'  \item{\code{\{\{\{ auth_accounts \}\}\}}}{- Adds a call to \link{ga_account_list} for the appropriate GA account type (GA4 or Universal)}
#'  \item{\code{\{\{\{ model_server \}\}\}}}{- Adds the server side module for the models as configured in the \link{ga_model} configuration. It uses the object loaded above via the model_load macro.  It looks like \code{model1$server('model1')} in the code.}
#'  \item{\code{\{\{\{ model1 \}\}\}}}{- Alternative to \code{model_load}, this will load the model file location instead, which you can pass to \code{ga_model_load()}} in the template.  model1 is the first model passed, model2 the second, etc.
#'  \item{\code{\{\{\{ your_argument \}\}\}}}{- You can pass in your own custom variables to the template via the \code{...} argument of this function if they are named the same as the template macro variable}
#' }
#' 
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
#' # see an example of an ui.R template with macros
#' ga_model_shiny_template("basic/ui.R", read_lines = TRUE)
#' 
#' # see an example of an app.R template with macros
#' ga_model_shiny_template("basic_app/app.R", read_lines = TRUE)
#' 
#' \dontrun{
#' 
#' # a universal analytics model using default template "basic"
#' ga_model_shiny(
#'   ga_model_example("decomp_ga.gamr"), 
#'   auth_dropdown = "universal")
#'
#' # a template from a directory holding an app.R file
#' ga_model_shiny(
#'   ga_model_example("decomp_ga.gamr"), 
#'   auth_dropdown = "universal",
#'   template = ga_model_shiny_template("basic_app"))
#'   
#'   
#' # a template from only an ui.R file that will import boilerplate server.R
#' ga_model_shiny(
#'   ga_model_example("decomp_ga.gamr"), 
#'   auth_dropdown = "universal",
#'   template = ga_model_shiny_template("basic/ui.R"))
#'
#' # a template from a custom html based theme
#' ga_model_shiny(
#'   ga_model_example("decomp_ga.gamr"), 
#'   auth_dropdown = "universal",
#'   template = ga_model_shiny_template("html_based"))
#' 
#' # a template using library(argonDash)
#' ga_model_shiny(
#'   ga_model_example("ga-effect.gamr"), 
#'   title = "Argon Demo",
#'   auth_dropdown = "universal",
#'   template = ga_model_shiny_template("argonDash") )
#' 
#' # multiple models
#' m3 <- ga_model_example("time-normalised.gamr")
#' m4 <- ga_model_example("ga-effect.gamr")
#' 
#' # launch in gentelella template
#' ga_model_shiny(list(m4, m3), auth_dropdown = "universal",
#'               template = ga_model_shiny_template("gentelella"))
#' 
#'      
#' # you can make custom ui embedded within the template file
#' # use \{\{\{ model_list \}\}\} to work with the models in the ui.R
#' 
#' # below adds custom macro 'theme' and a custom ui in box tabs
#' ga_model_shiny(list(m4, m3), auth_dropdown = "universal", 
#'                template = ga_model_shiny_template("shinythemes"), 
#'                theme = "yeti")
#'
#' # shinydashboard's custom ui functions put a model in each side tab      
#' ga_model_shiny(list(m4, m3), auth_dropdown = "universal", 
#'                template = ga_model_shiny_template("shinydashboard"), 
#'                skin = "green")
#'                
#' # send in lots of theme variables to bslib in shiny > 1.6.0
#' ga_model_shiny(list(m4, m3), auth_dropdown = "universal",
#'                template = ga_model_shiny_template("basic_bslib"), 
#'                bg = "white", fg = "red", primary = "grey")
#' }
#' 
#' 
#' 
#' @family GA modelling functions
ga_model_shiny <- function(
  models,
  template = ga_model_shiny_template("basic"),
  header_boilerplate = TRUE,
  title = "ga_model_shiny",
  auth_dropdown = c("ga4","universal","none"),
  web_json = Sys.getenv("GAR_CLIENT_WEB_JSON"),
  date_range = TRUE,
  scopes = "https://www.googleapis.com/auth/analytics.readonly",
  deployed_url = "",
  local_folder = "",
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
  
  model_template <- make_model_template(
    model_locations, 
    date_range = date_range)
  
  txt <- ga_model_shiny_template_make(
    template, 
    header_boilerplate = header_boilerplate)
  
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
            level = 2)
  
  render <- lapply(txt, whisker.render, data = values)

  if(nzchar(local_folder)){
    write_template_object(render, local_folder)
    # copy over any dependencies in template folder
    file.copy(list.files(template, full.names = TRUE), 
              local_folder, recursive = TRUE, overwrite = FALSE)   
    return(invisible(render))
  }
  
  tmp_dir <- tempdir()
  if(dir.exists(template)){
    # copy over any dependencies in template folder
    file.copy(list.files(template, full.names = TRUE), 
              tmp_dir, recursive = TRUE, overwrite = FALSE)    
  }

  write_template_object(render, tmp_dir)
  
  myMessage("Launching Shiny app from", tmp_dir, level = 3)
  
  shiny_obj <- create_app_from_template(render, tmp_dir)
  shiny::runApp(shiny_obj)
}

create_app_from_template <- function(output, location){
  if(!is.null(output$app) && nzchar(output$app)){
    myMessage("Detected Shiny app.R for location:", location, level = 3)
    return(shiny::shinyAppDir(location))
  }
  
  # a shiny app in location with ui.R and server.R
  ui <- source(file.path(location, "ui.R"), chdir = TRUE)
  server <- source(file.path(location, "server.R"), chdir = TRUE)

  shiny::shinyApp(
    googleAuthR::gar_shiny_ui(ui$value, 
                              login_ui = googleAuthR::silent_auth), 
    server$value)
  
}

write_template_object <- function(output, destination_folder){
  if(!dir.exists(destination_folder)){
    dir.create(destination_folder)
  }

  if(!is.null(output$app) && nzchar(output$app)){
    loc <- file.path(destination_folder, "app.R")
    myMessage("Writing Shiny app.R to", loc, level = 2)
    writeLines(output$app, loc)
  }
  
  if(!is.null(output$ui) && nzchar(output$ui)){
    loc <- file.path(destination_folder, "ui.R")
    myMessage("Writing Shiny ui.R to", loc, level = 2)
    writeLines(output$ui, loc)
  }
  
  if(!is.null(output$server) && nzchar(output$server)){
    loc <- file.path(destination_folder, "server.R")
    myMessage("Writing Shiny server.R to", loc, level = 2)
    writeLines(output$server, loc)
  }
  
  invisible(NULL)
  
}

# turn templates files into txt for server.R and ui.R for launching
ga_model_shiny_template_make <- function(template, header_boilerplate = TRUE){
  
  if(!dir.exists(template) && !file.exists(template)){
    stop("Couldn't detect if template was a file or directory: ", template, 
         call. = FALSE)
  }
  
  # as an example
  output <- list(
    app = NULL,
    ui = NULL,
    server = NULL
  )
  
  hdr_txt <- ""
  if(header_boilerplate){
    myMessage(
      "Adding ga_model_shiny_template('header_boilerplate.R') to Shiny code",
      level = 2)
    # add the header boiler plate
    hdr_txt <- ga_model_shiny_template("boilerplate/header_boilerplate.R", 
                                       read_lines = TRUE)
  }
  
  # default
  server_txt <- ga_model_shiny_template("boilerplate/server_boilerplate.R",
                                        read_lines = TRUE)
  
  if(dir.exists(template)){
    # its a directory holding at least ui.R or app.R
    dir_files <- list.files(template, recursive = TRUE)
    
    if(!"ui.R" %in% dir_files && !"app.R" %in% dir_files){
      stop("Template folder must include ui.R or app.R file", call. = FALSE)
    }
    
    if("app.R" %in% dir_files){
      myMessage("Using app.R in template folder ", template)
      # does it have a server object? write it out as app.R
      app_txt <- readLines(file.path(template, "app.R"))
      output$app <- has_server_object(
        app_txt, 
        ga_model_shiny_template("boilerplate/server_app_boilerplate.R", 
                                read_lines = TRUE))
    } else {
      ui_txt <- readLines(file.path(template, "ui.R"))
      
      # it has its own server.R so overwrite default
      if("server.R" %in% dir_files){
        server_txt <- readLines(file.path(template, "server.R"))
      }
      
      output$ui <- c(hdr_txt, ui_txt)
      output$server <- server_txt
    }
    
  } else if(file.exists(template)){
    
    # its a file of app.R, ui.R or expression that produces a Shiny app object
    ui_txt <- readLines(template)

    # we assume all ui.R files do not include server objects
    if(basename(template) == "ui.R"){
      output$ui <- c(hdr_txt, ui_txt)
      output$server <- server_txt
    } else {
      # does it have a server object? write it out as app.R
      output$app <- has_server_object(
        ui_txt, 
        ga_model_shiny_template("boilerplate/server_app_boilerplate.R", 
                                read_lines = TRUE))
    }

  }
  
  output

}

has_server_object <- function(txt, default){
  
  # does it have a server object? 
  if(any(grepl("^server", txt))){
    return(txt)
  }
  
  default
}

make_date_range <- function(date_range){
  if(!date_range) return("br()")
  
  list(date_range = 
         'dateRangeInput("date_range", "Date Range", 
          start = Sys.Date() - 400, end = Sys.Date() - 1)')
}

make_model_libraries <- function(models){
  the_libs <- unique(unlist(lapply(models, function(x) x$required_packages)))
  list(
    model_libraries = paste(sprintf("library(%s)", the_libs), collapse = "\n")
  )
}

make_model_template <- function(model_locations, date_range){
  
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
  
  # models in a list that can be used in theme templates
  model_list <- paste("list(", 
                      paste(names(model_locations), 
                            collapse = ",", sep = ","),
                      ")")
  
  list(
    model_load = paste(
      sprintf("%s <- ga_model_shiny_load('%s')", 
              names(model_locations), model_locations), 
      collapse = "\n"),
    model_ui = shiny_ui(names(model_locations)),
    model_server = model_server,
    model_list = model_list
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
#' @examples 
#' 
#' ga_model_shiny_ui("model1")
#' @keywords internal
#' @noRd
shiny_ui <- function(model_n){
  paste(
    sprintf("%s$ui('%s')", model_n, model_n), 
    collapse = ",\n")
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

