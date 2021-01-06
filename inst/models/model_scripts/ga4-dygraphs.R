# fetch data
data_f <- function(view_id,
                   date_range = c("400daysAgo","yesterday"),
                   metrics = c("sessions"),
                   ...)
{
  
  ga_data(view_id, 
          metrics = metrics, 
          date_range = date_range, 
          dimensions = "date", 
          limit = -1, 
          orderBys = ga_data_order(+date))
  
}

# model data
model_f <- function(df,
                    ...)
{
  
  xts::xts(df[, -1], order.by = df$date)
  
}

# output data
output_f<- function(df,...)
{
  require(dygraphs)
  dygraph(df, main = "GA4 trend- googleAnalyticsR") %>% 
    dyAxis("x", label = "Date") %>%
    dyOptions(axisLineWidth = 1.5, drawGrid = FALSE)
  
}

model <- ga_model_make(
  data_f = data_f, 
  required_columns = c("date"), 
  model_f = model_f, 
  output_f = output_f, 
  required_packages = c("xts","dygraphs"), 
  description = "GA4 Metric Trend", 
  outputShiny = dygraphs::dygraphOutput, 
  renderShiny = dygraphs::renderDygraph
  )
