#' Create named list of allowed GA metrics/dimensions
#' 
#' @param type Type of parameter to create
#' @param subType to restrict to only those in this type
#' @param callAPI This will update the meta table (Requires online authorization)
#' 
#' This is useful to expand goalXCompletions to all the possibilities,
#'   as well as restricting to those that variables that work with your API call.
#' 
#' Use internal meta table, but you have option to update to the latest version.
#'   
#' @return A named list of parameters for use in API calls
#' @export
allowed_metric_dim <- function(type = c("METRIC", "DIMENSION"),
                               subType = c("all","segment","cohort"),
                               callAPI = FALSE){
  type <- match.arg(type)
  subType <- match.arg(subType)
  
  if(callAPI){
    meta <- google_analytics_meta()
  } else {
    meta <- googleAnalyticsR::meta
  }
  
  ## where to restrict what dims/metrics are chosen from, todo
  filtered_md <- switch(subType,
                        all = meta,
                        segment = meta[grepl("true",meta$allowedInSegments),],
                        cohort = meta[grepl("Lifetime Value and Cohort", meta$group),])
  
  ## only public (not deprecated) varaibles
  filtered_md <- filtered_md[filtered_md$type == type & 
                               filtered_md$status == "PUBLIC",]
  
  ## replace XX with 1 to 20
  meta_ex <- filtered_md[grepl("XX",filtered_md$name),]
  
  f <- function(y) vapply(1:20, function(x) gsub("XX", x, meta_ex$name[y]), character(1))
  meta_expanded <- unlist(lapply(seq_along(meta_ex$name), f))
  
  ## repeat with names
  f2 <- function(y) vapply(1:20, function(x) gsub("XX", x, meta_ex$uiName[y]), character(1))
  meta_expanded_names <- unlist(lapply(seq_along(meta_ex$uiName), f2))
  
  
  ## take out XX from filtered_md
  out <- c(filtered_md$name[!filtered_md$name %in% meta_ex$name],
           meta_expanded)
  names(out) <- c(filtered_md$uiName[!filtered_md$uiName %in% meta_ex$uiName],
                  meta_expanded_names)
  
  out
}


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