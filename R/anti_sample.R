## get request with sampling
anti_sample <- function(viewId,
                        date_range,
                        metrics,
                        dimensions,
                        dim_filters,
                        met_filters,
                        filtersExpression,
                        order,
                        segments,
                        pivots,
                        cohorts,
                        metricFormat,
                        histogramBuckets){

  test_call <- google_analytics_4(viewId            = viewId,
                                  date_range        = date_range,
                                  metrics           = metrics,
                                  dimensions        = dimensions,
                                  dim_filters       = dim_filters,
                                  met_filters       = met_filters,
                                  filtersExpression = filtersExpression,
                                  order             = order,
                                  segments          = segments,
                                  pivots            = pivots,
                                  cohorts           = cohorts,
                                  max               = 10,
                                  metricFormat      = metricFormat,
                                  samplingLevel     = "LARGE",
                                  histogramBuckets  = histogramBuckets)
  
  read_counts <- as.integer(attr(test_call,"samplesReadCounts")[[1]])
  space_size <- as.integer(attr(test_call, "samplingSpaceSizes")[[1]])
  samplingPer <- get_samplePercent(read_counts, space_size)
  
  if(identical(samplingPer, numeric(0))){
    message("No sampling found, returning call")
    return(test_call)
  }
  
  ## sampling
  explore_sessions <- google_analytics_4(viewId = viewId,
                                         date_range = date_range,
                                         metrics = "sessions",
                                         dimensions = "date")
  explore_sessions$cumalitve <- cumsum(explore_sessions$sessions)
  explore_sessions$sample_bucket <- as.factor((explore_sessions$cumalitve %/% read_counts) + 1)
  splits <- split(explore_sessions, explore_sessions[["sample_bucket"]])
  
  new_date_ranges <- lapply(splits, function(x) {list(start_date = min(x$date), 
                                                     end_date = max(x$date),
                                                     range_date = nrow(x))})
  
  ## if any new_date_ranges range_date is 1 then possibily will still sample.
  
  ## send to fetch
  message("Calculated that [", length(new_date_ranges), "] batches are needed to avoid sampling, assuming [", read_counts, "] sampleReadCounts from a [", space_size, "] samplingSpaceSize.")
  
  unsampled_list <- lapply(new_date_ranges, function(x){
    message("Batch covering ", x$range_date, " days: ", x$start_date, ", ", x$end_date)
    us_call <- google_analytics_4(viewId            = viewId,
                                  date_range        = c(x$start_date, x$end_date),
                                  metrics           = metrics,
                                  dimensions        = dimensions,
                                  dim_filters       = dim_filters,
                                  met_filters       = met_filters,
                                  filtersExpression = filtersExpression,
                                  order             = order,
                                  segments          = segments,
                                  pivots            = pivots,
                                  cohorts           = cohorts,
                                  metricFormat      = metricFormat,
                                  samplingLevel     = "LARGE",
                                  max               = 99999999,
                                  histogramBuckets  = histogramBuckets)
    
  })
  
## if sampled, get samplingSpaceSizes / samplesReadCounts rounded up is number of batches needed.

## divide up the call into batches using date, hour and minute.
## end_date - start_date is number of days

## divide number of days by batches

## make new requests with date filter

## if number of  split call using hours

## if number of  split call using minutes

## if number of  can't help you. Fetch what you can.


## if response unsampled, keep

## if repoonse still sampled, feed back into function

## output response

## rbind with other responses
}