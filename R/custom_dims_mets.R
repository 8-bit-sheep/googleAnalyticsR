#' Get Custom Dimensions or Metrics
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param type A customMetric or customDimension
#' @param customId The customMetricId or customDimensionId
#'
#' @return Custom Metric or Dimension meta data
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_custom_vars <- function(accountId,
                           webPropertyId,
                           type = c("customMetrics", "customDimensions"),
                           customId){
  
  type <- match.arg(type)
  
  pa <- switch(type,
               customMetrics =  list(
                 accounts = accountId,
                 webproperties = webPropertyId,
                 customMetrics = customId
               ),
               customDimensions = list(
                 accounts = accountId,
                 webproperties = webPropertyId,
                 customDimensions = customId
               )
  )
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  cus_var <- gar_api_generator(url,
                               "GET",
                               path_args = pa,
                               data_parse_function = function(x) x)
  
  cus_var()
  
}

#' List Custom Dimensions or Metrics
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param type A customMetric or customDimension
#'
#' @return Custom Metric or Dimension List
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
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