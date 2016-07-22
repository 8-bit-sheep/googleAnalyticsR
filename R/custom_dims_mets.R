#' List Custom Dimensions or Metrics
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param type A customMetric or customDimension
#'
#' @return AdWords Links
#' @importFrom googleAuthR gar_api_generator
#' @export
ga_custom_vars_list <- function(accountId,
                                webPropertyId,
                                type = c("customMetrics", "customDimensions")){
  
  type <- match.arg(type)
  
  pa <- switch(type,
      customMetrics =  list(
        accounts = accountId,
        webproperties = webPropertyId,
        customMetrics = ""
      ),
      customDimensions = list(
        accounts = accountId,
        webproperties = webPropertyId,
        customDimensions = ""
      )
    )
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  cus_var <- gar_api_generator(url,
                               "GET",
                               path_args = pa,
                               data_parse_function = function(x) x)
  
  cus_var()
  
}