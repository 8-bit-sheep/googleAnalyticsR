# ga_model: Performs decomposition and creates a plot

# no libraries needed

# fetch data
data_f <- function(view_id, date_range = c(Sys.Date() - 300, Sys.Date()), ...) {
    google_analytics(view_id, 
                     date_range = date_range, 
                     metrics = "sessions", dimensions = "date", 
                     max = -1)
}

# model data
model_f <- function(df, ...) {
    decompose(ts(df$sessions, frequency = 7))
}

# output data
output_f <- function(df, ...){
  plot(df)
  }

# use via ga_model_make()
