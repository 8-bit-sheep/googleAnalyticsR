batchData <- function(ga, ga_pars){
  ## TODO: max results currently ignored if over 10000, change that.
  ga_pars$`max-results` <- 10000
  myMessage("Finding number of results to fetch", level = 2)
  check_data <- ga(pars_arguments = ga_pars)
  batches <- attr(check_data, "totalResults") %/% 10000
  
  anyBatchSampled <- attr(check_data, 'containsSampledData')
  samplePercent   <- attr(check_data, "samplePercent")
  
  if(batches > 0){
    myMessage("Batching data into [", batches+1, "] calls.", level = 3)
    
    batch_start_index <- seq(1, (batches+1)*10000, 10000)
    
    anyBatchSampled <- FALSE
    samplePercent   <- 0
    
    bf <- function(batch_data){
      lapply(batch_data, function(the_data) {
        if(attr(the_data, 'containsSampledData')) anyBatchSampled <<- TRUE
        samplePercent <<- samplePercent + attr(the_data, "samplePercent")
      })
      batch_data
    }
    
    the_data <- googleAuthR::gar_batch_walk(ga,
                                            batch_start_index,
                                            gar_pars = ga_pars,
                                            pars_walk = "start-index",
                                            batch_function = bf)
    
  } else{
    the_data <- check_data
  }
  
  myMessage("All data found. [",
          NROW(the_data), "] total results out of a possible [",
          attr(the_data, "totalResults") , "]", level = 3)
  
  attr(the_data, "containsSampledData") <- anyBatchSampled
  attr(the_data, "samplePercent") <- samplePercent / (batches + 1)
  the_data
  
}

walkData <- function(ga, ga_pars, start, end){
  dates <- as.character(
    seq(as.Date(start, format="%Y-%m-%d"),
        as.Date(end, format="%Y-%m-%d"),
        by=1))
  
  ga_pars$samplingLevel <- "HIGHER_PRECISION"
  ga_pars$`max-results` <- 10000
  
  anyBatchSampled <- FALSE
  samplePercent   <- 0
  
  bf <- function(batch_data){
    lapply(batch_data, function(the_data) {
      if(attr(the_data, 'containsSampledData')) anyBatchSampled <<- TRUE
      samplePercent <<- samplePercent + attr(the_data, "samplePercent")
    })
    batch_data
  }
  
  walked_data <- googleAuthR::gar_batch_walk(ga,
                                             dates,
                                             gar_pars = ga_pars,
                                             pars_walk = c("start-date", "end-date"),
                                             batch_function = bf)
  
  myMessage("Walked through all dates. Total Results: [", NROW(walked_data), "]", level = 3)
  attr(walked_data, "dateRange") <- list(startDate = start, endDate = end)
  attr(walked_data, "totalResults") <- NROW(walked_data)
  attr(walked_data, "samplingLevel") <- "HIGHER_PRECISION, WALKED"
  attr(walked_data, "containsSampledData") <- anyBatchSampled
  attr(walked_data, "samplePercent") <- samplePercent / length(dates)
  
  walked_data
  
}


loop_ids <- function(id, ga_pars, samplingLevel, max_results, ga){
  all_data <- list()
  for(i in id){
    ga_pars$ids <- i
    
    ## if walk through results then split up dates and walk through date ranges
    if(samplingLevel %in% "WALK"){
      myMessage("Walking through data.", level = 3)
      the_data <- walkData(ga, ga_pars, ga_pars$`start-date`, ga_pars$end)
      
    } else {
      ## No batch needed
      if(max_results < 10000) {
        the_data <- ga(pars_arguments = ga_pars)
      } else {
        ## batching data
        the_data <- batchData(ga, ga_pars)
      }
      
    }
    
    if(length(id) > 1){
      ## for multiple id's, a list of dataframes.
      myMessage("Multiple IDs", level = 2)
      id_name <- attr(the_data, "profileInfo")$profileId
      
      all_data[[id_name]] <- the_data
      
    } else {
      ## for one id, just a dataframe
      all_data <- the_data
      
    }
    
  }
  
  all_data
}