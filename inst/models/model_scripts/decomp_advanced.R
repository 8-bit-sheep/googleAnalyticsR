# ga_model: Performs decomposition and creates a plot

# no libraries needed

# fetch data
data_f <- function(view_id, 
                   date_range = c(Sys.Date() - 300, Sys.Date()), 
                   metric = "sessions", 
    ...) {
    o <- google_analytics(view_id, 
                          date_range = date_range, 
                          metrics = metric, dimensions = "date", 
        max = -1)
    o$the_metric <- o[, metric]
    o
}

# model data
model_f <- function(df, frequency, ...) {
    decompose(ts(df$the_metric, frequency = as.numeric(frequency)))
}

# output data
output_f <- function(df, ...){
    plot(df)
}


# use via ga_model_make()
