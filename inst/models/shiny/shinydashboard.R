library(shinydashboard)

header <- dashboardHeader(
  title = "{{ shiny_title }}",
  titleWidth = 250
)
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem("Logout", href = "/"),
    # custom macro for shiny dashboard
    {{{ model_tabs }}}
  )
)
body <- dashboardBody(
  fluidRow(
    box(width=12,
        {{ auth_ui }},
        {{{ date_range }}}
        )
    ),
  tabItems(
    {{{ model_ui }}}
  )
)

skin <- "{{ skin }}"
if(!nzchar(skin)) skin <- "blue"

## ui.R
ui <- dashboardPage(header, sidebar, body, skin = skin)
