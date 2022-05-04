#' Run a GA4 Data Funnel report
#' 
#' Returns a customized funnel report of your Google Analytics event data. The data returned from the API is as a table with columns for the requested dimensions and metrics.
#' 
#' @inheritParams ga_data
#' @param open_funnel In an open funnel, users can enter the funnel in any step, and in a closed funnel, users must enter the funnel in the first step. Optional. If unspecified, a closed funnel is used.
#' @param funnel_steps A list of FunnelStep objects
#' 
#' @export
#' @family GA4 functions
ga_data_funnel <- function(funnel_steps, 
                           propertyId, 
                           open_funnel = FALSE, 
                           breakdown_dim = NULL, 
                           breakdown_limit = 5L, 
                           next_action = NULL,
                           visual = "STANDARD_FUNNEL",
                           dim_filters = NULL,
                           limit = 10000L){
  
}


#' Create a FunnelStep object for use within ga_data_funnel
#' 
#' @param name The distinctive name for this step. If unspecified, steps will be named by a 1 based indexed name (i.e. "0. ", "1. ", etc.).
#' @param directly_followed If true, this step must directly follow the previous step
#' @param fun_filter A funnel filter expression - the condition that your users must meet to be included in this step of the funnel journey.
#' @param within_duration If specified, this step must complete within this duration of the completion of the prior step. 
#' 
#' @family GA4 functions
#' @export 
ga_data_funnel_step <- function(fun_filter, name, within_duration, directly_followed){
  
}