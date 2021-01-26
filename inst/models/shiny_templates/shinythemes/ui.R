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
  tabPanels <- mapply(function(x,y) tabPanel(y$model$description, y$ui(x)), 
                      x=model_n, y=models, 
                      SIMPLIFY = FALSE)
  do.call(tabsetPanel, args = unname(tabPanels))
}

navbarPage(
  title = "{{ shiny_title }}",
  theme = the_theme,
  tabPanel("Models",
    {{ auth_ui }},
    sidebarPanel(
      {{{ date_range }}}
    ),
    mainPanel(
      shinytheme_tabPanel(models)
    )
  )
)



