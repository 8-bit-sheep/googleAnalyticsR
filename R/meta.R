#' Get current dimensions and metrics available in GA API.
#'
#' @return dataframe of dimensions and metrics available to use
#'
#' @seealso \url{https://developers.google.com/analytics/devguides/reporting/metadata/v3/reference/metadata/columns/list}
#' 
#' @importFrom googleAuthR gar_api_generator
#' 
#' @export
google_analytics_meta <- function(){
  
  meta <- gar_api_generator("https://www.googleapis.com/analytics/v3",
                            "GET",
                            path_args = list(metadata = "ga",
                                             columns = ""),
                            data_parse_function = parse_google_analytics_meta )
  
  meta()
  
}