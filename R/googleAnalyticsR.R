#' Library for getting Google Analytics data into R
#' 
#' @description 
#' 
#' \if{html}{
#'   \figure{gar_logo_1000.png}{googleAnalyticsR}
#' }
#' 
#' Follow the online documentation here: \url{https://code.markedmondson.me/googleAnalyticsR/}
#' 
#' @details 
#' 
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
                                                       "id",
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