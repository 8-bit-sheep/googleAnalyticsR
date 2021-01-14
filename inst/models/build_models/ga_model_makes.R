source("inst/models/model_scripts/ua-causalimpact.R")
ga_model_edit("inst/models/ga-effect.gamr",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = dygraphs::dygraphOutput, 
              renderShiny = dygraphs::renderDygraph, inputShiny = uiInput)

source("inst/models/model_scripts/ga4-dygraphs.R")
ga_model_edit("inst/models/ga4-trend.gamr",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = dygraphs::dygraphOutput, 
              renderShiny = dygraphs::renderDygraph, inputShiny = uiInput)

date_input <- shiny::dateRangeInput("date_range", "Dates", 
                                    start = Sys.Date()-300, end = Sys.Date()-1)
source("inst/models/model_scripts/ua-decomp.R")
ga_model_edit("inst/models/decomp_ga.gamr",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = shiny::plotOutput, 
              renderShiny = shiny::renderPlot,
              inputShiny = date_input)

date_input <- shiny::dateRangeInput("date_range", "Dates", 
                                    start = Sys.Date()-300, end = Sys.Date()-1)
freq_input <- shiny::selectInput("frequency", "Periodic Frequency",
                                 choices = c(7,28,365))
metric_input <- shiny::selectInput("metric", "Metric", 
                                   choices = c("sessions","users","pageviews"))
source("inst/models/model_scripts/decomp_advanced.R")
ga_model_edit("inst/models/decomp_ga_advanced.gamr", 
              description = "Performs decomposition (Advanced)",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = shiny::plotOutput, renderShiny = shiny::renderPlot,
              inputShiny = shiny::tagList(date_input, freq_input))

source("inst/models/model_scripts/ua-time-normalised.R")

library(shiny)
is <- tagList(
  numericInput("first_day", "First day minimum pageviews",
                               value = 2, min=0, max=100),
  numericInput("total_min_cutoff", "Minimum Total pageviews",
                             value = 500, min = 0, max = 1000),
  numericInput("days_live", label = "Days Live",
                               value = 60, min = 10, max = 400),
  textInput("page_regex", label = "Page filter regex", value = ".*")
)

ga_model_edit("inst/models/time-normalised.gamr",
              data_f = data_f, model_f = model_f, output_f = output_f, 
              outputShiny = plotly::plotlyOutput, 
              renderShiny = plotly::renderPlotly,
              inputShiny = is)

