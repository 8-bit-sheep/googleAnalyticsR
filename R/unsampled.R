#' List Unsampled Reports
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return Unsampled Reports List
#' @importFrom googleAuthR gar_api_generator
#' @export
ga_goals_list <- function(accountId,
                          webPropertyId,
                          profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  unsampled <- gar_api_generator(url,
                                 "GET",
                                 path_args = list(
                                   accounts = accountId,
                                   webproperties = webPropertyId,
                                   profiles = profileId,
                                   unsampledReports = ""
                                 ),
                                 data_parse_function = function(x) x)
  
  unsampled()
  
}