#' Experiments Meta data
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param experimentId Experiment Id
#'
#' @return Experiment Meta Data
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_experiment <- function(accountId,
                          webPropertyId,
                          profileId,
                          experimentId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  experiments <- gar_api_generator(url,
                                   "GET",
                                   path_args = list(
                                     accounts = accountId,
                                     webproperties = webPropertyId,
                                     profiles = profileId,
                                     experiments = experimentId
                                   ),
                                   data_parse_function = function(x) x)
  
  experiments()
  
}

#' List Experiments
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return Experiments List
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_experiment_list <- function(accountId,
                               webPropertyId,
                               profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  experiments <- gar_api_generator(url,
                                   "GET",
                                   path_args = list(
                                     accounts = accountId,
                                     webproperties = webPropertyId,
                                     profiles = profileId,
                                     experiments = ""
                                   ),
                                   data_parse_function = parse_ga_experiments_list)
  
  pages <- gar_api_page(experiments, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' @noRd
#' @import assertthat
parse_ga_experiments_list <- function(x){
  
  o <- x %>% 
    management_api_parsing("analytics#experiments") 
  
  if(is.null(o)){
    return(data.frame())
  }
  
  o <- o %>% 
    mutate(created = iso8601_to_r(created),
           updated = iso8601_to_r(updated),
           startTime = iso8601_to_r(startTime),
           endTime = iso8601_to_r(endTime))
    
  attr(o, "nextLink") <- x$nextLink
  o
  
}