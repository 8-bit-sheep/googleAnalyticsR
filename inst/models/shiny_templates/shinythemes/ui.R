library(shinythemes)
## basic ui.R
theme <- "{{ theme }}"
if(!nzchar(theme)) theme <- "cerulean"

the_theme <- shinytheme(theme)

# a list of the model objects
models <- {{ model_list }}

# uses models (a list of models) to create the model UI 
shinytheme_tabPanel <- function(models){
  model_n <- paste0("model", seq_along(models)) 
  tabs <- mapply(
    function(x,y) shiny::tabPanel(y$model$description, y$ui(x)), 
    x=model_n, y=models, 
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  do.call(shiny::tabsetPanel, args = tabs)
}

shiny::navbarPage(
  title = "{{ shiny_title }}",
  theme = the_theme,
  shiny::tabPanel("Models",
    {{ auth_ui }},
    shiny::sidebarPanel(
      {{{ date_range }}}
    ),
    shiny::mainPanel(
      shinytheme_tabPanel(models)
    )
  )
)



