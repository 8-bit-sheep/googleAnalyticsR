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
             segmentBuilderUI("demo_segments")
             ),
      column(width = 6,
             multi_selectUI("metric_seg", "Metric", width = "100%"),
             multi_selectUI("dim_seg", "Dimensions", width = "100%"),
             dateRangeInput("date_seg", "Date Range", start = Sys.Date() - 30, width = "100%"),
             actionButton("get_seg", "Fetch GAv4 Segment Data"),
             dataTableOutput("segment_table")
           )),
  tabPanel("Calculated Metrics", tabName = "calc_metrics", icon = icon("calculator")),
  tabPanel("Pivots", tabName = "pivots", icon = icon("sort-amount-desc")),
  tabPanel("Multiple Dates", tabName = "multi_dates", icon = icon("calendar")),
  tabPanel("Cohorts", tabName = "cohorts", icon = icon("users"))
)
