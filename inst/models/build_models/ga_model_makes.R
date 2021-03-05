library(googleAnalyticsR)
library(dygraphs)
library(xts)
library(shiny)
library(plotly)
library(CausalImpact)

source("inst/models/model_scripts/ua-causalimpact.R")
m1 <- ga_model_make(
  data_f = data_f, model_f = model_f, output_f = output_f,
  required_columns = c("date","channelGrouping","sessions"),
  required_packages = c("CausalImpact","xts","tidyr",
                        "googleAnalyticsR","assertthat","dygraphs"),
  description = "Causal Impact on channelGrouping data ",
  outputShiny = dygraphs::dygraphOutput, 
  renderShiny = dygraphs::renderDygraph, 
  inputShiny = shiny::dateInput("event_date", 
                                label = "Event Date",
                                value = Sys.Date() - 30))
ga_model_save(m1, "inst/models/examples/ga-effect.gamr")

source("inst/models/model_scripts/ga4-dygraphs.R")

uiInput <- shiny::selectInput("metrics", 
                              label = "Pick a Metric",
                              choices = c("sessions","newUsers","conversions"),
                              multiple = TRUE)
ga_model_edit("inst/models/examples/ga4-trend.gamr",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = dygraphs::dygraphOutput, 
              renderShiny = dygraphs::renderDygraph, 
              inputShiny = uiInput)

source("inst/models/model_scripts/ua-decomp.R")

m4 <- ga_model_make(
  data_f = data_f, model_f = model_f, output_f = output_f, 
  description = "Perform decomposition on your GA sessions",
  required_columns = c("date","sessions"),
  required_packages = c("googleAnalyticsR"),
  outputShiny = shiny::plotOutput, 
  renderShiny = shiny::renderPlot,
  inputShiny = shiny::tagList())
ga_model_save(m4, "inst/models/examples/decomp_ga.gamr")

date_input <- shiny::dateRangeInput("date_range", "Dates", 
                                    start = Sys.Date()-300, end = Sys.Date()-1)
freq_input <- shiny::selectInput("frequency", "Periodic Frequency",
                                 choices = c(7,28,365))
metric_input <- shiny::selectInput("metric", "Metric", 
                                   choices = c("sessions","users","pageviews"))
source("inst/models/model_scripts/decomp_advanced.R")
ga_model_edit("inst/models/examples/decomp_ga_advanced.gamr", 
              description = "Performs decomposition (Advanced)",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = shiny::plotOutput, renderShiny = shiny::renderPlot,
              inputShiny = shiny::tagList(date_input, freq_input))

source("inst/models/model_scripts/ua-time-normalised.R")


is <- shiny::tagList(
  shiny::numericInput("first_day_pageviews_min", "First day minimum pageviews",
                               value = 2, min=0, max=100),
  shiny::numericInput("total_unique_pageviews_cutoff", "Minimum Total pageviews",
                             value = 500, min = 0, max = 1000),
  shiny::numericInput("days_live_range", label = "Days Live",
                               value = 60, min = 10, max = 400),
  shiny::textInput("page_filter_regex", label = "Page filter regex", value = ".*")
)

ga_model_edit("inst/models/examples/time-normalised.gamr",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = plotly::plotlyOutput, 
              renderShiny = plotly::renderPlotly,
              inputShiny = is)

