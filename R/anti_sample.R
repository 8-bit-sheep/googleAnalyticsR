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

  myMessage("Finding how much sampling in data request...", level = 3)
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
  rowCount <- as.integer(attr(test_call, "rowCount")[[1]])
  ## adds 10% to read_counts as its flaky
  m_error <- 0.9 
  
  if(identical(samplingPer, numeric(0))){
    message("No sampling found, returning call")
    return(test_call)
  }
  
  ## sampling
  myMessage("Finding number of sessions for anti-sample calculations...", level = 3)
  explore_sessions <- google_analytics_4(viewId = viewId,
                                         date_range = date_range,
                                         metrics = "sessions",
                                         dimensions = "date")
  explore_sessions$cumulative <- cumsum(explore_sessions$sessions)
  explore_sessions$sample_bucket <- as.factor((explore_sessions$cumulative %/% (read_counts*m_error) + 1))
  splits <- split(explore_sessions, explore_sessions[["sample_bucket"]])
  
  new_date_ranges <- lapply(splits, function(x) {list(start_date = min(x$date), 
                                                     end_date = max(x$date),
                                                     range_date = nrow(x))})
  
  ## if any new_date_ranges range_date is 1 then possibily will still sample.
  ## split by hour/day
  
  ## send to fetch
  myMessage("Calculated [", length(new_date_ranges), "] batches are needed to download [", rowCount,"] rows unsampled.", level = 3)
  myMessage("Found [", read_counts, "] sampleReadCounts from a [", space_size, "] samplingSpaceSize.", level = 2)
  
  did_it_work <- TRUE
  unsampled_list <- lapply(new_date_ranges, function(x){
    myMessage("Anti-sample call covering ", x$range_date, " days: ", x$start_date, ", ", x$end_date, level = 3)
    out <- google_analytics_4(viewId            = viewId,
                              date_range        = c(x$start_date,x$end_date),
                              metrics           = metrics,
                              dimensions        = dimensions,
                              dim_filters       = dim_filters,
                              met_filters       = met_filters,
                              filtersExpression = filtersExpression,
                              order             = order,
                              segments          = segments,
                              pivots            = pivots,
                              cohorts           = cohorts,
                              max               = rowCount,
                              metricFormat      = metricFormat,
                              samplingLevel     = "LARGE",
                              histogramBuckets  = histogramBuckets)
    read_counts2 <- as.integer(attr(out,"samplesReadCounts")[[1]])
    space_size2 <- as.integer(attr(out, "samplingSpaceSizes")[[1]])
    samplingPer <- get_samplePercent(read_counts2, space_size2)
    if(!identical(samplingPer, numeric(0))){
      myMessage("Anti-sampling failed", level = 3)
      did_it_work <<- FALSE
    }
    out
  })
  
  out <- Reduce(rbind, unsampled_list)
  attr(out, "totals") <- NULL
  attr(out, "minimums") <- NULL
  attr(out, "maximums") <- NULL
  attr(out, "rowCount") <- as.character(nrow(out))
  attr(out, "nextPageToken") <- NULL
  myMessage("Finished unsampled data request, total rows [", nrow(out),"]", level = 3)
  if(did_it_work) myMessage("Successfully avoided sampling", level = 3)
  out
}