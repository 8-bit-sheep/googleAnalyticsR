# ga_model: A description

# no libraries needed

# fetch data
data_f <-
function (view_id, date_range = c(Sys.Date() - 300, Sys.Date()), 
    ...) 
{
    google_analytics(view_id, date_range = date_range, metrics = "sessions", 
        dimensions = "date", max = -1)
}

# model data
model_f <-
function (df, ...) 
{
    decompose(ts(df$sessions, frequency = 7))
}

# output data
output_f <-
function (df, ...) 
plot(df)

# shiny input function
uiInput <-
structure(list(), names = character(0), class = c("shiny.tag.list", 
"list"))

# use via ga_model_make()
