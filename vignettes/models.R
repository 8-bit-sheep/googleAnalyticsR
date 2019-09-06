## ----setup, include=FALSE------------------------------------------------
library(googleAnalyticsR)
ga_cache_call("cache")

## ----message=FALSE, warning=FALSE----------------------------------------
library(prophet)
library(googleAnalyticsR)
library(googleAnalyticsModelR)

ga_auth(email = Sys.getenv("GARGLE_EMAIL"))
my_view_id <- 81416156

forecast_data <- ga_model_prophet(my_view_id, interactive_plot = TRUE)
forecast_data$plot

## ----message=FALSE, warning=FALSE----------------------------------------
output <- ga_time_normalised(my_view_id, interactive_plot = TRUE)
output$plot

## ----message=FALSE, warning=FALSE----------------------------------------
library(googleAnalyticsR)  # load library

# authenticate
ga_auth(email = Sys.getenv("GARGLE_EMAIL"))

# your own Google Analytics viewID
my_viewid <- 81416156

# load the model (equivalent to ga_model_load())
decomp_ga <- ga_model_example("decomp_ga.gamr")

# apply model to your data
d1 <- ga_model(my_viewid, model = decomp_ga)

## ----message=FALSE, warning=FALSE----------------------------------------
# change default date range to 20 days ago to yesterday
d2 <- ga_model(my_viewid, model = decomp_ga, date_range = c("20daysAgo","yesterday"))

## ------------------------------------------------------------------------
decomp_ga

## ------------------------------------------------------------------------
d2

## ------------------------------------------------------------------------
get_model_data <- function(viewId,
                           date_range = c(Sys.Date()- 300, Sys.Date()),
                           metric,
                           ...){
   o <- google_analytics(viewId,
                    date_range = date_range,
                    metrics = metric,
                    dimensions = "date",
                    max = -1)
    # rename the metric column so its found for modelling
    o$the_metric <- o[, metric]
    
    o
    
 }

decompose_sessions <- function(df, frequency, ...){
   decompose(ts(df$the_metric, frequency = frequency))
 }
 
decomp_ga_advanced <- ga_model_make(get_model_data,
                           required_columns = c("date"), # less restriction on column
                           model_f = decompose_sessions,
                           output_f = graphics::plot,
                           description = "Performs decomposition and creates a plot",
                           outputShiny = shiny::plotOutput,
                           renderShiny = shiny::renderPlot)
                           

## ----message=FALSE, warning=FALSE----------------------------------------
result <- ga_model(81416156, decomp_ga_advanced, metric="users", frequency = 30)

## ------------------------------------------------------------------------
decomp_ga_advanced

## ----message=FALSE, warning=FALSE----------------------------------------
ga_model_edit(decomp_ga_advanced, description = "New description")

## ------------------------------------------------------------------------
str(decomp_ga_advanced, give.attr = FALSE)

## ------------------------------------------------------------------------
decomp_ga_advanced$data_f


decomp_ga_advanced$description

## ------------------------------------------------------------------------
get_ci_data <- function(viewId, 
                        date_range = c(Sys.Date()-600, Sys.Date()),
                        ...){
  
  google_analytics(viewId, 
                   date_range = date_range,
                   metrics = "sessions",
                   dimensions = c("date", "channelGrouping"), 
                   max = -1)
}

## ------------------------------------------------------------------------
# response_dim is the channel to predict.
# predictors help with forecast
do_ci <- function(df, 
                  event_date,
                  response = "Organic Search",
                  predictors = c("Video","Social","Direct"),
                  ...){
  
  message("CausalImpact input data columns: ", paste(names(df), collapse = " "))
  # restrict to one response 
  stopifnot(is.character(response), 
            length(response) == 1,
            assertthat::is.date(event_date),
            is.character(predictors))
  
  pivoted <- df %>% 
    tidyr::spread(channelGrouping, sessions)
  
  stopifnot(response %in% names(pivoted))
  
  ## create a time-series zoo object
  web_data_xts <- xts::xts(pivoted[-1], order.by = as.Date(pivoted$date), frequency = 7)
  
  pre.period <- as.Date(c(min(df$date), event_date))
  post.period <- as.Date(c(event_date + 1, max(df$date)))
  
  predictors <- intersect(predictors, names(web_data_xts))

  ## data in order of response, predictor1, predictor2, etc.
  model_data <- web_data_xts[,c(response,predictors)]
  
  # deal with names
  names(model_data) <- make.names(names(model_data))
  # remove any NAs
  model_data[is.na(model_data)] <- 0

  CausalImpact::CausalImpact(model_data,  pre.period, post.period)

}

## ------------------------------------------------------------------------
dygraph_plot <- function(impact, event_date, ...){

  ## the data for the plot is in here
  ci <- impact$series
  
  ci <- xts::xts(ci)

  ## the dygraph output
  dygraph(data=ci[,c('response', 
                     'point.pred', 'point.pred.lower', 'point.pred.upper')], 
          main="Expected (95% confidence level) vs Observed", group="ci") %>%
    dyEvent(x = event_date, "Event") %>%
    dySeries(c('point.pred.lower', 'point.pred','point.pred.upper'), 
             label='Expected') %>%
    dySeries('response', label="Observed")
}

## ------------------------------------------------------------------------
req_packs <- c("CausalImpact", "xts", "tidyr", "googleAnalyticsR", "assertthat", "dygraphs")

## ------------------------------------------------------------------------
ci_model <- ga_model_make(get_ci_data,
                          required_columns = c("date","channelGrouping","sessions"),
                          model_f = do_ci,
                          output_f = dygraph_plot,
                          required_packages = req_packs,
                          description = "Causal Impact on channelGrouping data",
                          outputShiny = dygraphs::dygraphOutput,
                          renderShiny = dygraphs::renderDygraph)
# print out model details
ci_model

## ----message=FALSE, warning=FALSE----------------------------------------
library(googleAnalyticsR)
library(CausalImpact)
library(xts)
library(tidyr)
library(dygraphs)

ga_auth(email = Sys.getenv("GARGLE_EMAIL"))

ci <- ga_model(81416156, ci_model, event_date = as.Date("2019-01-01"))

# print to show the plot object
ci$plot

