#' Get a GA request with anti-sampling
#' 
#' Calculates multiple API calls to avoid sampling
#' 
#' @inheritParams make_ga_4_req
#' @inheritParams google_analytics_4
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
                        histogramBuckets,
                        slow_fetch){
  
  if(length(date_range) > 2) stop("Anti-sampling not available for comparison date ranges.")
  
  ## stop bug where batch sizes break
  if(!is.null(date_range)){
    
    ## issue 112 - support "today" and "yesterday"
    ## to lower - we want to allow Today and Yesterday or TODAY and YESTERDAY
    date_range <- gsub("^today$", Sys.Date(), tolower(date_range))
    date_range <- gsub("^yesterday$", Sys.Date() - 1, tolower(date_range))   
    
    date_range <- as.Date(date_range)
  }
  
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
                                  histogramBuckets  = histogramBuckets,
                                  slow_fetch        = FALSE)
  

  ## reduce read counts by 10% to get more calls as returned figure is flakey
  read_counts <- round(as.integer(attr(test_call,"samplesReadCounts")[[1]]) * 0.9)
  space_size  <- as.integer(attr(test_call, "samplingSpaceSizes")[[1]])
  samplingPer <- get_samplePercent(read_counts, space_size)
  
  ## add 20% to rowCount as its flakey (sampled rows of 0 not included?)
  rowCount <- round(as.integer(attr(test_call, "rowCount")[[1]]) * 1.2)
  
  if(identical(samplingPer, numeric(0))){
    myMessage("No sampling found, returning call", level = 3)
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
                                    histogramBuckets  = histogramBuckets,
                                    anti_sample       = FALSE,
                                    slow_fetch        = slow_fetch)
    return(unsampled)
  }
  
  if(anti_sample_batches == "auto"){
    ## sampling
    myMessage("Finding number of sessions for anti-sample calculations...", level = 3)
    explore_sessions <- google_analytics_4(viewId = viewId,
                                           date_range = date_range,
                                           metrics = "sessions",
                                           dimensions = "date",
                                           max = -1) ## download all days! #66
    explore_sessions$cumulative <- cumsum(explore_sessions$sessions)
    explore_sessions$sample_bucket <- chunkify(explore_sessions$sessions, limit = 250e3)
    
  } else {
    stopifnot(anti_sample_batches >= 1)
    asb <- as.integer(anti_sample_batches)
    date_col <- seq(date_range[1], date_range[2], by = 1)
    sample_bucket <- as.factor((seq_along(date_col) - 1) %/% asb) # 0 indexed lengths (#74)
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
                              histogramBuckets  = histogramBuckets,
                              slow_fetch        = slow_fetch)
    
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
  
  ## get rid of duplicate rows per sample call
  out <- aggregateGAData(out, agg_names = gsub("ga:","",dimensions))
  
  ## fill these in later
  if(!is.null(out)){
    attr(out, "totals") <- NULL
    attr(out, "minimums") <- NULL
    attr(out, "maximums") <- NULL
    attr(out, "rowCount") <- as.character(nrow(out))
    attr(out, "nextPageToken") <- NULL
    attr(out, "antiSampleWorked") <- did_it_work
    myMessage("Finished unsampled data request, total rows [", nrow(out),"]", level = 3)
    if(did_it_work) myMessage("Successfully avoided sampling", level = 3)
  }

  out
}


#' Break down a request into unsampled chunks
#' 
#' @param sessions_vec A vector, ordered by date, with the number of sessions
#' @param limit This is the upper bound for the number of sessions in one chunk
#'   
#' @return A vector with the batch number of each date. This allows for usage in
#'   a mutate.
#' @keywords internal
chunkify <- function(sessions_vec, limit = 250e3) {
  #Accumulators
  batch_size    <- 0
  batch_number  <- 1
  batch_numbers <- rep(1, length(sessions_vec))
  
  for (i in 1:length(sessions_vec)) {
    sessions <- sessions_vec[i]
    batch_size <- batch_size + sessions
    
    #If today puts up above the limit, it starts a new batch!
    if (batch_size >= limit) {
      batch_number <- batch_number + 1
      batch_size   <- sessions
    }
    
    batch_numbers[i] <- batch_number
  }
  
  batch_numbers
}
