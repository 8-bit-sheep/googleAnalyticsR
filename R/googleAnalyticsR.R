#' Library for getting Google Analytics data into R
#' 
#' @description 
#' 
#' \ifelse{html}{
#'   \figure{logo.png}{googleAnalyticsR}
#' }{\strong{googleAnalyticsR}}
#' 
#' Follow the online documentation here: <https://code.markedmondson.me/googleAnalyticsR/>
#' 
#' 
#' @docType package
#' @name googleAnalyticsR
#' @keywords internal
NULL

## horrible hack https://github.com/tidyverse/magrittr/issues/29
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".",
                           "account",
                           "accountName",
                           "account_name",
                           "accountId",
                           "activityType",
                           "apiName",
                           "ecommerce",
                           "products",
                           "transactionId",
                           "webPropertyId",
                           "websiteUrl",
                           "viewName",
                           "viewId",
                           "sessionId",
                           "activityTime",
                           "cd_value",
                           "cd_index",
                           "displayName",
                           "hit_data",
                           "has_goal",
                           "goals",
                           "adWordsAccounts",
                           "childLink.type",
                           "childLink.href",
                           "created",
                           "endTime",
                           "entity.accountRef.id",
                           "entity.webPropertyRef.id",
                           "entity.profileRef.webPropertyId",
                           "entity.profileRef.id",
                           "entity.webPropertyRef.kind",
                           "entity.webPropertyRef.href",
                           "id",
                           "kind",
                           "linkId", 
                           "parentLink.href",
                           "parentLink.type",
                           "permissions.local",
                           "property",
                           "propertyId",
                           "property_name",
                           "selfLink",
                           "startTime",
                           "uiName",
                           "updated",
                           "userRef.email",
                           "uploadTime",
                           "userRef.kind"))
  
} 
