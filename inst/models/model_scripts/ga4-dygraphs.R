# ga_model: GA4 Metric Trend

library(xts)
library(dygraphs)

# fetch data
data_f <- function(view_id, date_range = c("400daysAgo", "yesterday"), metrics = c("sessions"), 
    ...) {
    ga_data(view_id, metrics = metrics, date_range = date_range, dimensions = "date", 
        limit = -1, orderBys = ga_data_order(+date))
}

# model data
model_f <- function(df, ...) {
    xts::xts(df[, -1], order.by = df$date)
}

# output data
output_f <- function(df, ...) {
    require(dygraphs)
    dygraph(df, main = "GA4 trend- googleAnalyticsR") %>% dyAxis("x", label = "Date") %>% 
        dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
}

# use via ga_model_make()
