#' Get a GA request with anti-sampling
#' 
#' Calculates multiple API calls to avoid sampling
#' 
#' @inheritParams make_ga_4_req
#' @keywords internal
anti_sample <- function(anti_sample_batches,
                        viewId,
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
  
  if(length(date_range) > 2) stop("Anti-sampling not available for comparison date ranges.")
  
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
  

  ## reduce read counts by 10% to get more calls as returned figure is flakey
  read_counts <- round(as.integer(attr(test_call,"samplesReadCounts")[[1]]) * 0.9)
  space_size  <- as.integer(attr(test_call, "samplingSpaceSizes")[[1]])
  samplingPer <- get_samplePercent(read_counts, space_size)
  
  ## add 20% to rowCount as its flakey (sampled rows of 0 not included?)
  rowCount <- round(as.integer(attr(test_call, "rowCount")[[1]]) * 1.2)
  
  if(identical(samplingPer, numeric(0))){
    message("No sampling found, returning call")
    unsampled <- google_analytics_4(viewId            = viewId,
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
                                    max               = -1,
                                    metricFormat      = metricFormat,
                                    samplingLevel     = "LARGE",
                                    histogramBuckets  = histogramBuckets)
    return(unsampled)
  }
  
  if(anti_sample_batches == "auto"){
    ## sampling
    myMessage("Finding number of sessions for anti-sample calculations...", level = 3)
    explore_sessions <- google_analytics_4(viewId = viewId,
                                           date_range = date_range,
                                           metrics = "sessions",
                                           dimensions = "date")
    explore_sessions$cumulative <- cumsum(explore_sessions$sessions)
    explore_sessions$sample_bucket <- as.factor((explore_sessions$cumulative %/% read_counts) + 1)
    
  } else {
    stopifnot(anti_sample_batches >= 1)
    asb <- as.integer(anti_sample_batches)
    date_col <- seq(date_range[1], date_range[2], by = 1)
    sample_bucket <- as.factor((1:length(date_col) %/% asb) + 1)
    explore_sessions <- data.frame(date = date_col, sample_bucket = sample_bucket)
  }
  
  ## split to find new date ranges
  splits <- split(explore_sessions, explore_sessions[["sample_bucket"]])

  new_date_ranges <- lapply(splits, function(x) {list(start_date = min(x$date), 
                                                     end_date = max(x$date),
                                                     range_date = nrow(x))})
  myMessage("Calculated [", length(new_date_ranges), "] batches are needed to download approx. [", rowCount,"] rows unsampled.", 
            level = 3)
  myMessage("Found [", read_counts, "] sampleReadCounts from a [", space_size, "] samplingSpaceSize.", 
            level = 2)
  
  ## send to fetch
  did_it_work <- TRUE
  unsampled_list <- lapply(new_date_ranges, function(x){
    
    if(x$range_date > 1){
      
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
      
    } else {
      ## if any new_date_ranges range_date is 1 then possibily will still sample.
      myMessage("Attempting hourly anti-sampling...", level = 3)
      out <- hourly_anti_sample(viewId            = viewId,
                                the_day           = x$start_date,
                                metrics           = metrics,
                                dimensions        = dimensions,
                                dim_filters       = dim_filters,
                                met_filters       = met_filters,
                                filtersExpression = filtersExpression,
                                order             = order,
                                segments          = segments,
                                pivots            = pivots,
                                cohorts           = cohorts,
                                max               = attr(out, "rowCount"),
                                metricFormat      = metricFormat,
                                histogramBuckets  = histogramBuckets,
                                read_counts = read_counts)
    }
    
    read_counts2 <- as.integer(attr(out,"samplesReadCounts")[[1]])
    space_size2  <- as.integer(attr(out, "samplingSpaceSizes")[[1]])
    samplingPer2  <- get_samplePercent(read_counts2, space_size2)
    
    if(!identical(samplingPer2, numeric(0))){
      myMessage("Anti-sampling failed", level = 3)
      did_it_work <<- FALSE
    }
    
    out
  })
  
  out <- Reduce(rbind, unsampled_list)
  
  ## fill these in later
  attr(out, "totals") <- NULL
  attr(out, "minimums") <- NULL
  attr(out, "maximums") <- NULL
  attr(out, "rowCount") <- as.character(nrow(out))
  attr(out, "nextPageToken") <- NULL
  myMessage("Finished unsampled data request, total rows [", nrow(out),"]", level = 3)
  if(did_it_work) myMessage("Successfully avoided sampling", level = 3)
  out
}

#' hourly get request with anti-sampling
#' @keywords internal
hourly_anti_sample <- function(viewId,
                               the_day,
                               metrics,
                               dimensions,
                               dim_filters,
                               met_filters,
                               filtersExpression,
                               order,
                               segments,
                               max,
                               pivots,
                               cohorts,
                               metricFormat,
                               histogramBuckets,
                               read_counts){
  
  ## get session distribution per hour
  ## sampling
  myMessage("Finding number of hourly sessions for anti-sample calculations...", level = 3)
  explore_sessions <- google_analytics_4(viewId = viewId,
                                         date_range = c(the_day, the_day),
                                         metrics = "sessions",
                                         dimensions = "hour")
  
  ## work out batches
  explore_sessions$cumulative <- cumsum(explore_sessions$sessions)
  explore_sessions$sample_bucket <- as.factor((explore_sessions$cumulative %/% read_counts) + 1)
  splits <- split(explore_sessions, explore_sessions[["sample_bucket"]])
  
  new_hour_ranges <- lapply(splits, function(x) {list(hours = x$hour,
                                                      range_date = nrow(x))})

  
  ## do calls
  
  all_samplesReadCounts <- 0
  all_samplingSpaceSizes <- 0
  unsampled_list <- lapply(new_hour_ranges, function(x){

    myMessage("Anti-sample call covering ", x$range_date, " hours: ", paste(x$hours, collapse = " "), 
              level = 3)
    
    hour_filter <- filter_clause_ga4(list(dim_filter("hour", "IN_LIST", x$hours)))
    
    if(!is.null(dim_filters)){
      stop("Can't use dim_filters argument with hourly anti-sampling, sorry!  
           Can you use filtersExpression instead?")
    }
    
    out <- google_analytics_4(viewId            = viewId,
                              date_range        = c(the_day, the_day),
                              metrics           = metrics,
                              dimensions        = dimensions,
                              dim_filters       = hour_filter,
                              met_filters       = met_filters,
                              filtersExpression = filtersExpression,
                              order             = order,
                              segments          = segments,
                              pivots            = pivots,
                              cohorts           = cohorts,
                              max               = max,
                              metricFormat      = metricFormat,
                              samplingLevel     = "LARGE",
                              histogramBuckets  = histogramBuckets)
    
    read_counts3 <- as.integer(attr(out,"samplesReadCounts")[[1]])
    space_size3  <- as.integer(attr(out, "samplingSpaceSizes")[[1]])
    samplingPer  <- get_samplePercent(read_counts3, space_size3)
    
    if(!identical(samplingPer, numeric(0))){
      myMessage("Hourly anti-sampling failed. Holy-moly, you should try GA 360 and BigQuery.", level = 3)
      all_samplesReadCounts  <<- all_samplesReadCounts + read_counts3
      all_samplingSpaceSizes <<- all_samplingSpaceSizes + space_size3
    }
    out
  })
  ## output rbind
  hour_out <- Reduce(rbind, unsampled_list)
  attr(hour_out, "samplesReadCounts")  <- all_samplesReadCounts
  attr(hour_out, "samplingSpaceSizes") <- all_samplingSpaceSizes
  hour_out
}