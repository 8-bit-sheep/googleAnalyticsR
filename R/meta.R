#' Get current dimensions and metrics available in GA API.
#'
#' @param version The Google Analytics API metadata to fetch - "universal" for Universal and earlier versions, "data" for Google Analytics 4
#' @return dataframe of dimensions and metrics available to use
#'
#' @seealso \url{https://developers.google.com/analytics/devguides/reporting/metadata/v3/reference/metadata/columns/list}, \url{https://developers.google.com/analytics/trusted-testing/analytics-data/rest/v1alpha/TopLevel/getUniversalMetadata}
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
#' }
ga_meta <- function(version = c("universal","data")){
  
  version <- match.arg(version)
  
  if(version == "universal"){
    meta <- gar_api_generator("https://www.googleapis.com/analytics/v3",
                              "GET",
                              path_args = list(metadata = "ga",
                                               columns = ""),
                              data_parse_function = parse_google_analytics_meta )
  } else if(version == "data"){
    meta <- ga_meta_aw
  }

  
  meta()
  
}

ga_meta_aw <- function(){
  
  the_url <- sprintf("https://analyticsdata.googleapis.com/%s/universalMetadata",
              version_aw())
  meta <- gar_api_generator(the_url, "GET",
                            data_parse_function = parse_ga_meta_aw)
  
  meta()
  
}

