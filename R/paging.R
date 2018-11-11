#' A common paging function for GA API
#' @noRd
paging_function <- function(x){
  next_entry <- x$startIndex + x$itemsPerPage
  
  # we have all results e.g. 1001 > 1000
  if(next_entry > x$totalResults){
    return(NULL)
  }
  
  next_entry
}