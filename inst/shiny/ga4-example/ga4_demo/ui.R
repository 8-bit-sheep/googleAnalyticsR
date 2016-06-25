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
             actionButton("get_seg", "Fetch Segment Data"),
             dataTableOutput("segment_table")
           )),
  tabPanel("Calculated Metrics", tabName = "calc_metrics", icon = icon("calculator")),
  tabPanel("Pivots", tabName = "pivots", icon = icon("sort-amount-desc")),
  tabPanel("Multiple Dates", tabName = "multi_dates", icon = icon("calendar"),
           multi_selectUI("metric_md", "Metric"),
           multi_selectUI("dim_md", "Dimensions"),
           dateRangeInput("date1_md", "Date Range 1", start = Sys.Date() - 30),
           dateRangeInput("date2_md", "Date Range 2", start = Sys.Date() - 60, end = Sys.Date() - 31),
           actionButton("get_md", "Fetch Multidate Data"),
           dataTableOutput("md_table")
           ),
  tabPanel("Cohorts", tabName = "cohorts", icon = icon("users"),
    column(width = 6,
           multi_selectUI("metric_coh", "Metric"),
           multi_selectUI("dim_coh", "Dimensions"),
           textInput("coh1", label = "Cohort 1 Name", value = "Cohort 1"),
           dateRangeInput("date_coh1", "Cohort 1 Range", start = Sys.Date() - 30, end = Sys.Date()),
           textInput("coh2", label = "Cohort 2 Name", value = "Cohort 2"),
           dateRangeInput("date_coh2", "Cohort 2 Range", start = Sys.Date() - 60, end = Sys.Date() - 30),
           textInput("coh3", label = "Cohort 3 Name", value = "Cohort 3"),
           dateRangeInput("date_coh3", "Cohort 3 Range", start = Sys.Date() - 90, end = Sys.Date() - 60),
           textInput("coh4", label = "Cohort 4 Name", value = "Cohort 4"),
           dateRangeInput("date_coh4", "Cohort 4 Range", start = Sys.Date() - 120, end = Sys.Date() - 90)
           ),
    column(width = 6,
           actionButton("get_cohort", "Fetch Cohorts Data"),
           dataTableOutput("cohort_table"))
           )
)
