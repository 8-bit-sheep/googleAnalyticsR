#' Library for getting Google Analytics data into R
#' 
#' Follow the online documentation here: \url{http://code.markedmondson.me/googleAnalyticsR/}
#' 
#' \itemize{
#'   \item First Google Analytics Reporting v4 API library for R
#'   \item v4 features include: dynamic calculated metrics, pivots, 
#'     histograms, date comparisons, batching
#'   \item Multi-user login in Shiny App
#'   \item Integration with BigQuery Google Analytics Premium/360 exports
#'   \item Single authentication flow with other 'googleAuthR' apps
#'   \item Automatic batching
#'   \item sampling avoidance with daily walk
#'   \item multi-account fetching
#'   \item multi-channel funnnels
#'   \item Support for batch. For big data calls this could 
#'     be 10x quicker than normal GA fetching
#'   \item Meta data included in attributes of returned dataframe
#'  }
#'  
#' You may wish to set the below environment arguments for easier authentication
#' 
#' \code{
#' GA_CLIENT_ID
#' GA_CLIENT_SECRET
#' GA_WEB_CLIENT_ID
#' GA_WEB_CLIENT_SECRET
#' GA_AUTH_FILE
#' }
#' 
#' @docType package
#' @name googleAnalyticsR
NULL

## horrible hack https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1") utils::globalVariables(c("."))