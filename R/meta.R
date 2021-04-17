#' Get current dimensions and metrics available in GA API.
#'
#' @param version The Google Analytics API metadata to fetch - "universal" for Universal and earlier versions, "data" for Google Analytics 4
#' @param propertyId If requesting from Google Analytics 4, pass the propertyId to get metadata specific to that property.  Leaving it NULL or 0 will return universal metadata
#' @param cached Whether to use a cached version or to use the API to fetch the results again
#' @param no_api Don't call the API, just return `googleAnalyticsR::meta4`
#' @return dataframe of dimensions and metrics available to use
#'
#' @seealso <https://developers.google.com/analytics/devguides/reporting/metadata/v3/reference/metadata/columns/list>, <https://developers.google.com/analytics/devguides/reporting/data/v1/rest/v1alpha/properties/getMetadata>
#' 
#' @importFrom googleAuthR gar_api_generator
#' 
#' @export
#' @examples 
#' 
#' \dontrun{
#' 
#' # universal analytics
#' ga_meta()
#' 
#' # Google Analytics 4 metadata from the Data API
#' ga_meta("data")
#' 
#' # Google Analytics 4 metadata for a particular Web Property
#' ga_meta("data", propertyId = 206670707)
#' 
#' }
ga_meta <- function(version = c("universal","data"),
                    propertyId = NULL,
                    cached = TRUE,
                    no_api = FALSE){
  
  version <- match.arg(version)
  
  if(version == "universal"){
    meta <- gar_api_generator("https://www.googleapis.com/analytics/v3",
                              "GET",
                              path_args = list(metadata = "ga",
                                               columns = ""),
                              data_parse_function = parse_google_analytics_meta )
    o <- meta()
  } else if(version == "data"){
    if(no_api) return(googleAnalyticsR::meta4)
    
    if(cached && 
       !is.null(.ga_meta_env$meta) 
       && !is.null(propertyId)
       && attr(.ga_meta_env$meta, "propertyId") == propertyId){
      myMessage("Cached meta data ", level = 2)
      return(.ga_meta_env$meta)
    }
    o <- ga_meta_data(propertyId)
  }

  o
  
  
}

.ga_meta_env <- new.env(parent = globalenv())
.ga_meta_env$meta <- NULL

ga_meta_data <- function(propertyId){
  
  pid <- 0
  if(!is.null(propertyId)){
    pid <- propertyId
    myMessage("Metadata for propertyId", pid, level = 3)
  }
  
  the_url <- sprintf("https://analyticsdata.googleapis.com/%s/properties/%s/metadata",
              version_aw(), pid)
  meta <- gar_api_generator(the_url, "GET",
                            data_parse_function = parse_ga_meta_aw)
  
  o <- meta()
  attr(o, "propertyId") <- pid
  myMessage("Caching new meta data", level = 3)
  .ga_meta_env$meta <- o
  
  o
  
}

