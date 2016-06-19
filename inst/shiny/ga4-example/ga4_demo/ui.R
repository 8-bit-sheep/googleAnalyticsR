library(shiny)
library(googleAuthR)
library(googleAnalyticsR)
options(shiny.port = 1221)
source("modules.R")

navbarPage("GA v4 API",
  tabPanel("Setup", tabName = "setup", icon = icon("cogs"),
           googleAuthUI("login"),
           authDropdownUI("auth_menu")
           ),
  tabPanel("Segments", tabName = "segments", icon = icon("object-group"),
      column(width = 6,
             segmentBuilderUI("demo_segments")),
      column(width = 6,
             br())
           ),
  tabPanel("Calculated Metrics", tabName = "calc_metrics", icon = icon("calculator")),
  tabPanel("Pivots", tabName = "pivots", icon = icon("sort-amount-desc")),
  tabPanel("Multiple Dates", tabName = "multi_dates", icon = icon("calendar")),
  tabPanel("Cohorts", tabName = "cohorts", icon = icon("users"))
)

# 
# header <- dashboardHeader(
#   title = "GAv4 API Demo"
#   
# )
# 
# sidebar <- dashboardSidebar(
#   sidebarMenu(
#     menuItem(googleAuthUI("login")),
#     menuItem("Setup", tabName = "setup", icon = icon("cogs")),
#     menuItem("Segments", tabName = "segments", icon = icon("object-group")),
#     menuItem("Calculated Metrics", tabName = "calc_metrics", icon = icon("calculator")),
#     menuItem("Pivots", tabName = "pivots", icon = icon("sort-amount-desc")),
#     menuItem("Multiple Dates", tabName = "multi_dates", icon = icon("calendar")),
#     menuItem("Cohorts", tabName = "cohorts", icon = icon("users"))
#   )
# )
# 
# body <- dashboardBody(
#   tabItems(
#     tabItem("setup",
#              fluidRow(
#                box(width = 12,
#                    authDropdownUI("auth_menu")
#                ))
#     ),
#     tabItem("segments",
#       box(width = 8, height = 1000,
#           segmentBuilderUI("demo_segments")  
# 
#       ),
#       column(width = 4,
#              br()
#       )
#     ),
#     tabItem("calc_metrics",
#       br()
#     ),
#     tabItem("pivots",
#              br()
#     ),
#     tabItem("multi_dates",
#       br()
#     ),
#     tabItem("cohorts",
#       br()       
#     )
#   )
# )
# 
# dashboardPage(header, sidebar, body, title = "Google Analytics API v4 Demo")
# 
