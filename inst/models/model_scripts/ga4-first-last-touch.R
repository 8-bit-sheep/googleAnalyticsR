data_f <- function(view_id, 
                   date_range = c("30daysAgo","yesterday"),
                   ){
  myMessage("Warning - this is an expensive API call, don't use it on too long a date range", level = 3)
  ga_data(view_id, 
          date_range = date_range, 
          metrics = "sessions", 
          dimensions = c("firstUserMedium", "sessionMedium"))
  
}

model_f <- function(df){
  
  nodes <- unique(c(ga_data$firstUserMedium, ga_data$sessionMedium))
  
  list(
    nodes = data.frame(
      name = unique(c(ga_data$firstUserMedium, ga_data$sessionMedium)),
      stringsAsFactors = FALSE),
    links = data.frame(
      source = df$firstUserMedium,
      target = df$sessionMedium,
      value = df$sessions,
      stringsAsFactors = FALSE
    )
  )
}

plot_f <- function(df){

}