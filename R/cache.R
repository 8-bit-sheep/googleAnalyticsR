#' Setup caching of API calls
#' 
#' Lets you cache API calls to disk
#' 
#' @param cache_location If \code{RAM} will save to memory, or specify a file folder location
#' 
#' @details 
#' 
#' By default this is turned on upon package load to \code{RAM}.  
#'   Should you want to cache calls to a folder then run this function to specify where.
#' 
#' @export
#' @importFrom googleAuthR gar_cache_setup
#' @import assertthat
#' @importFrom memoise cache_filesystem
ga_cache_call <- function(cache_location){
  
  if(cache_location == "RAM"){
    return(gar_cache_setup())
  }
  
  assert_that(
    is.writeable(cache_location)
  )
  
  f <- function(req){
    
    ga4 <- tryCatch(req$content$reports, error = function(x) NULL)
    data <-tryCatch(req$content$rows, error = function(x) NULL)
    
    if(!is.null(ga4) || !is.null(data)) return(TRUE)
    
    myMessage("No caching", level = 2)
    FALSE
    
    }
  
  
  gar_cache_setup(mcache = memoise::cache_filesystem(cache_location),
                  invalid_func = f)
  
}