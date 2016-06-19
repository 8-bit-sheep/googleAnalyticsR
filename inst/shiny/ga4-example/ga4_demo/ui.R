library(shiny)
library(googleAuthR)
library(googleAnalyticsR)
library(shinydashboard)
options(shiny.port = 1221)
source("modules.R")

header <- dashboardHeader(
  title = "GAv4 API Demo"
  
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(googleAuthUI("login"))
  )
)

body <- dashboardBody(
  tabsetPanel(id = "tabset",
    tabPanel("Setup",
             fluidRow(
               box(width = 12,
                   authDropdownUI("auth_menu")
               ))
    ),
    tabPanel("Segments",
      segmentBuilderUI("demo_segments")
      
    ),
    tabPanel("Calculated Metrics",
      br()
    ),
    tabPanel("Pivots",
             br()
    ),
    tabPanel("Multiple dates",
      br()
      
    ),
    tabPanel("Cohorts",
      br()       
    )
  )

  
)

dashboardPage(header, sidebar, body, title = "Google Analytics API v4 Demo", skin = "black")

