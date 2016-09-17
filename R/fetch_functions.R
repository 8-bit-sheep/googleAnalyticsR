loop_ids <- function(id, ga_pars, samplingLevel, max_results, ga){
  all_data <- list()
  for(i in id){
    ga_pars$ids <- i
    
    ## if walk through results then split up dates and walk through date ranges
    if(samplingLevel %in% "WALK"){
      message("Walking through data.")
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
      message("Multiple IDs")
      id_name <- attr(the_data, "profileInfo")$profileId
      
      all_data[[id_name]] <- the_data
      
    } else {
      ## for one id, just a dataframe
      all_data <- the_data
      
    }
    
  }
  
  all_data
}