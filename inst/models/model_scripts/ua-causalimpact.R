# ga_model: Causal Impact on channelGrouping data

library(CausalImpact)
library(xts)
library(tidyr)
library(googleAnalyticsR)
library(assertthat)
library(dygraphs)

# fetch data
data_f <- function(view_id, date_range = c(Sys.Date() - 600, Sys.Date()), ...) {
    google_analytics(view_id, date_range = date_range, metrics = "sessions", dimensions = c("date", 
        "channelGrouping"), max = -1)
}

# model data
model_f <- function(df, event_date, response = "Organic Search", predictors = c("Video", 
    "Social", "Direct"), ...) {
    message("CausalImpact input data columns: ", paste(names(df), collapse = " "))
    stopifnot(is.character(response), length(response) == 1, assertthat::is.date(event_date), 
        is.character(predictors))
    pivoted <- df %>% tidyr::spread(channelGrouping, sessions)
    stopifnot(response %in% names(pivoted))
    web_data_xts <- xts::xts(pivoted[-1], order.by = as.Date(pivoted$date), frequency = 7)
    pre.period <- as.Date(c(min(df$date), event_date))
    post.period <- as.Date(c(event_date + 1, max(df$date)))
    predictors <- intersect(predictors, names(web_data_xts))
    model_data <- web_data_xts[, c(response, predictors)]
    names(model_data) <- make.names(names(model_data))
    model_data[is.na(model_data)] <- 0
    CausalImpact::CausalImpact(model_data, pre.period, post.period)
}

# output data
output_f <- function(impact, event_date, ...) {
    ci <- impact$series
    ci <- xts::xts(ci)
    dygraph(data = ci[, c("response", "point.pred", "point.pred.lower", "point.pred.upper")], 
        main = "Expected (95% confidence level) vs Observed", group = "ci") %>% dyEvent(x = event_date, 
        "Event") %>% dySeries(c("point.pred.lower", "point.pred", "point.pred.upper"), 
        label = "Expected") %>% dySeries("response", label = "Observed")
}

# shiny input function
uiInput <- shiny::dateInput("event_date", "Event Date", Sys.Date() - 30)

# use via ga_model_make()
