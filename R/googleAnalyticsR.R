#' Library for getting Google Analytics data into R
#' 
#' @description 
#' 
#' \if{html}{
#'   \figure{gar_logo_1000.png}{googleAnalyticsR}
#' }
#' 
#' Follow the online documentation here: \url{http://code.markedmondson.me/googleAnalyticsR/}
#' 
#' @details 
#' 
#' \itemize{
#'   \item First \href{http://code.markedmondson.me/googleAnalyticsR/v4.html}{Google Analytics Reporting v4} API library for R
#'   \item v4 features include: dynamic calculated metrics, pivots, 
#'     histograms, date comparisons, batching
#'   \item Multi-user login in Shiny App to enable \href{http://code.markedmondson.me/googleAnalyticsR/shiny.html}{Google Analytics powered Shiny Apps}
#'   \item Integration with \href{http://code.markedmondson.me/googleAnalyticsR/big-query.html}{BigQuery Google Analytics Premium/360 exports}
#'   \item Single authentication flow with other \link{googleAuthR} apps
#'   \item Automatic batching
#'   \item Multi-channel funnnels
#'   \item Support for batch. For big data calls this could 
#'     be 10x quicker than normal GA fetching
#'   \item Meta data included in attributes of returned dataframe
#'   \item Automatic caching of API requests
#'   \item Management API features such as \href{http://code.markedmondson.me/googleAnalyticsR/management.html#unsampled_reports}{unsampled report downloads} and view filter setup
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
if(getRversion() >= "2.15.1") utils::globalVariables(c(".",
                                                       "parentLink.href",
                                                       "parentLink.type",
                                                       "created",
                                                       "updated",
                                                       "userRef.email",
                                                       "permissions.local",
                                                       "linkId",
                                                       "entity.accountRef.id",
                                                       "entity.webPropertyRef.id",
                                                       "entity.profileRef.webPropertyId",
                                                       "entity.profileRef.id",
                                                       "selfLink",
                                                       "uploadTime",
                                                       "childLink.type",
                                                       "childLink.href",
                                                       "startTime",
                                                       "endTime",
                                                       "kind",
                                                       "userRef.kind"))