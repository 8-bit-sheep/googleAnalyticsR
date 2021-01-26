# shinydashboard ui.R
library(shinydashboard)

models <- {{{ model_list }}}

shinydashboard_ui_menu <- function(models){
  model_n <- paste0("model", seq_along(models)) 
  labels <- lapply(models, function(x) substr(x$model$description, 0,14))
  
  f <- function(x,y){
    menuItem(text = x, tabName = y)
  }
  menus <- mapply(f, labels, model_n, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  menus <- c(menuItem("Logout", href = "/"), menus)
  
  do.call(sidebarMenu, args = menus)
}

shinydashboard_tabItems <- function(models){
  model_n <- paste0("model", seq_along(models)) 
  the_tabs <- mapply(function(x,y) tabItem(tabName = x, 
                                           y$ui(x)), 
                     x=model_n, y=models, 
                     SIMPLIFY = FALSE, USE.NAMES = FALSE)
  do.call(tabItems, args = the_tabs)
}

header <- dashboardHeader(
  title = "{{ shiny_title }}",
  titleWidth = 250
)
sidebar <- dashboardSidebar(
  width = 250,
  shinydashboard_ui_menu(models)
)
body <- dashboardBody(
  fluidRow(
    box(width=12,
        {{ auth_ui }},
        {{{ date_range }}}
        )
    ),
  shinydashboard_tabItems(models)
)

skin <- "{{ skin }}"
if(!nzchar(skin)) skin <- "blue"

## ui.R
ui <- dashboardPage(header, sidebar, body, skin = skin)
