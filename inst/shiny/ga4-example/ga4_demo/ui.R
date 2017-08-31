library(shiny)
library(googleAuthR)
library(googleAnalyticsR)
library(listviewer)


navbarPage("GA v4 API",
           tabPanel("Setup", tabName = "setup", icon = icon("cogs"),
                    h1("A tour of GA reporting API v4"),
                    helpText("A demonstration of some of the new features available over the traditional v3 API."),
                    helpText("Start by logging in to your Google Analytics account."),
                    googleAuthUI("login"),
                    authDropdownUI("auth_menu")
           ),
           tabPanel("Segments", tabName = "segments", icon = icon("object-group"),
                    fluidRow(
                      column(width = 8, offset = 2,
                             h2("Segment setup"),
                             segmentBuilderUI("demo_segments")
                      )
                    ),
                    fluidRow(
                      column(width = 8, offset = 2,
                             h2("GA Data to Segment"),
                             multi_selectUI("metric_seg", "Metric", width = "100%"),
                             multi_selectUI("dim_seg", "Dimensions", width = "100%"),
                             dateRangeInput("date_seg", "Date Range", start = Sys.Date() - 30, width = "100%"),
                             helpText("Segments are more in line to what can be configured in the GA interface."),
                             helpText("They are a lot more powerful, but complex to configure."),
                             actionButton("get_seg", "Fetch Segment Data", icon = icon("download"), class = "btn-success"),
                             hr(),
                             h2("Results"),
                             dataTableOutput("segment_table"),
                             hr(),
                             h2("JSON API object"),
                             helpText("The JSON object for segments is shown below for reference."),
                             jsoneditOutput("segment_object")
                      ))),
           tabPanel("Calculated Metrics", tabName = "calc_metrics", icon = icon("calculator"),
                    h2("Calculated Metrics"),
                    fluidRow(
                      column(width = 6,
                             textInput("calculated_name", label = "Calculated Name", value = "Sessions Per Pageview")
                      ),
                      column(width = 6,
                             textInput("calculated_exp", label = "Calculated Expression", value = "ga:sessions / ga:pageviews")   
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             multi_selectUI("metric_calc", "Normal Metrics")     
                      ),
                      column(width = 6,
                             multi_selectUI("dim_calc", "Dimensions")  
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             dateRangeInput("date_clac", "Date Range") 
                      ),
                      column(width = 6,
                             br()
                      )
                    ),
                    helpText("Calculated metrics let you create your own metrics on the fly"),
                    actionButton("get_calc", "Fetch Calculated Metric data", icon = icon("download"), class = "btn-success"),
                    hr(),
                    h2("Results"),
                    dataTableOutput("calc_table"),
                    hr(),
                    h2("JSON API object"),
                    helpText("The JSON object for calculated metrics is shown below for reference."),
                    jsoneditOutput("calc_object")
           ),
           tabPanel("Pivots", tabName = "pivots", icon = icon("sort-amount-desc"),
                    h2("Pivots"),
                    fluidRow(
                      column(width = 6,
                             multi_selectUI("metric_pivot", "Metric")   
                      ),
                      column(width = 6,
                             multi_selectUI("dim_pivot", "Dimensions")   
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             dateRangeInput("date_pivot", "Date Range") 
                      ),
                      column(width = 6,
                             br() 
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             multi_selectUI("metric_pivot2", "Pivot Metric") 
                      ),
                      column(width = 6,
                             multi_selectUI("dim_pivot2", "Pivot Dimension")  
                      )
                      
                    ),
                    
                    helpText("Pivot fetches reshape the data you can fetch from the API. Useful for making 'long' data 'wide', it means you don't have to perform more data transformation yourself."),
                    helpText("Examples include breaking out traffic sources per column, or event labels per category."),
                    actionButton("get_pivot", "Fetch Pivot Data", icon = icon("download"), class = "btn-success"),
                    hr(),
                    h2("Results"),
                    dataTableOutput("pivot_table"),
                    hr(),
                    h2("JSON API object"),
                    helpText("The JSON object for pivots is shown below for reference."),
                    jsoneditOutput("pivot_object")
           ),
           tabPanel("Multiple Dates", tabName = "multi_dates", icon = icon("calendar"),
                    h2("Multiple Dates"),
                    fluidRow(
                      column(width = 6,
                             multi_selectUI("metric_md", "Metric")
                      ),
                      column(width = 6,
                             multi_selectUI("dim_md", "Dimensions")
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             dateRangeInput("date1_md", "Date Range 1", start = Sys.Date() - 30)   
                      ),
                      column(width = 6,
                             dateRangeInput("date2_md", "Date Range 2", start = Sys.Date() - 60, end = Sys.Date() - 31)
                      )
                      
                    ),
                    
                    helpText("Multiple date range fetches let you do direct comparisons without needing two API calls."),
                    actionButton("get_md", "Fetch Multidate Data", icon = icon("download"), class = "btn-success"),
                    hr(),
                    h2("Results"),
                    dataTableOutput("md_table"),
                    hr(),
                    h2("JSON API object"),
                    helpText("The JSON object for multi-date is shown below for reference."),
                    jsoneditOutput("md_object")
           ),
           tabPanel("Cohorts", tabName = "cohorts", icon = icon("users"),
                    h2("Cohorts"),
                    fluidRow(
                      column(width = 6,
                             multi_selectUI("metric_coh", "Metric")  
                      ),
                      column(width = 6,
                             multi_selectUI("dim_coh", "Dimensions")  
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             textInput("coh1", label = "Cohort 1 Name", value = "Cohort 1")     
                      ),
                      column(width = 6,
                             dateRangeInput("date_coh1", "Cohort 1 Range", start = Sys.Date() - 30, end = Sys.Date())
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             textInput("coh2", label = "Cohort 2 Name", value = "Cohort 2")
                      ),
                      column(width = 6,
                             dateRangeInput("date_coh2", "Cohort 2 Range", start = Sys.Date() - 60, end = Sys.Date() - 30)
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             textInput("coh3", label = "Cohort 3 Name", value = "Cohort 3")
                      ),
                      column(width = 6,
                             dateRangeInput("date_coh3", "Cohort 3 Range", start = Sys.Date() - 90, end = Sys.Date() - 60) 
                      )
                    ),
                    fluidRow(
                      column(width = 6,
                             textInput("coh4", label = "Cohort 4 Name", value = "Cohort 4")   
                      ),
                      column(width = 6,
                             dateRangeInput("date_coh4", "Cohort 4 Range", start = Sys.Date() - 120, end = Sys.Date() - 90)
                      )
                    ),
                    helpText("Cohorts are unique to the v4 API, letting you examine user behaviour as it changes over time."),
                    actionButton("get_cohort", "Fetch Cohorts Data", icon = icon("download"), class = "btn-success"),
                    hr(),
                    h2("Results"),
                    dataTableOutput("cohort_table"),
                    hr(),
                    h2("JSON API object"),
                    helpText("The JSON object for cohorts is shown below for reference."),
                    jsoneditOutput("cohort_object")
           ),
           helpText("Copyright 2016 Sunholo Ltd. Released under MIT license."),
           helpText("Created using R, Shiny and ", a(href = "http://code.markedmondson.me/googleAnalyticsR", "googleAnalyticsR")),
           helpText(a(href = "http://code.markedmondson.me", "Blog")," | ", a(href = "http://twitter.com/HoloMarkeD", "@HoloMarkeD"))
)
