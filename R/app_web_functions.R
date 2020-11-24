#' @import assertthat
#' @noRd
gaw_dates <- function(date_range){
  # create up to 4 date ranges
  assert_that((length(date_range) %% 2) == 0,
              length(date_range) <= 8)
  
  dateRanges <- split(date_range, ceiling(seq_along(date_range)/2))
  
  unname(lapply(dateRanges, date_ga4))
}

gaw_metric <- function(metrics){
  unnamed_metrics <- metrics[names(metrics) == ""]
  named_metrics <- metrics[names(metrics) != ""]
  
  if(is.null(names(metrics))){
    unnamed_metrics <- metrics
  }
  
  all_metrics <- lapply(unnamed_metrics, function(x) Metric(name = x))
  
  if(length(named_metrics) > 0){
    metric_exp <- lapply(names(named_metrics), function(x){
      Metric(name = x, expression = unname(named_metrics[x]))
    })
    
    all_metrics <- c(all_metrics, metric_exp)
  }
  
  unname(all_metrics)

}

gaw_dimension <- function(dimensions, delimiter = "/"){

  if(is.null(dimensions)) return(NULL)
  unnamed_dims <- dimensions[names(dimensions) == ""]
  named_dims   <- dimensions[names(dimensions) != ""]
  
  if(is.null(names(dimensions))){
    unnamed_dims <- dimensions
  }
  
  all_dims <- lapply(unnamed_dims, function(x) Dimension(name = x))
  
  if(length(named_dims) > 0){
    dim_exp <- lapply(names(named_dims), function(x){
      Dimension(name = x, 
        dimensionExpression = list(
          concatenate = list(
            dimensionNames = strsplit(unname(named_dims[x]),delimiter)[[1]],
            delimiter = delimiter
          )
        )
      )
    })
    
    all_dims <- c(all_dims, dim_exp)
  }
  
  unname(all_dims)
}

quota_messages <- function(o){
  pq <- o$propertyQuota
  care_factor <- 0.5
  verbose <- getOption("googleAuthR.verbose") < 3

  # quota messages
  if(pq$tokensPerDay$consumed > (care_factor*pq$tokensPerDay$remaining) ||
     verbose){
    myMessage("tokensPerDay: Query Cost [", pq$tokensPerDay$consumed, 
              "] / Remaining [", pq$tokensPerDay$remaining,"]",
              level = 3)
  }
  
  if(pq$tokensPerHour$consumed > (care_factor*pq$tokensPerHour$remaining) ||
     verbose){
    myMessage("tokensPerHour: Query Cost [", pq$tokensPerHour$consumed, 
              "] / Remaining [", pq$tokensPerHour$remaining,"]",
              level = 3)
  }
  
  if(pq$concurrentRequests < 10 || verbose){
    myMessage("concurrentRequests: ", pq$concurrentRequests, 
              " / 10",
              level = 3)
  }
  
  if(pq$serverErrorsPerProjectPerHour < 10 || verbose){
    myMessage("serverErrorsPerProjectPerHour: ", pq$serverErrorsPerProjectPerHour, 
              " / 10",
              level = 3)
  }
}

