#' Library for getting Google Analytics data into R
#' 
#' docs: https://developers.google.com/analytics/trusted-testing/reporting/
#' 
#' https://developers.google.com/analytics/trusted-testing/reporting/rest/v4/reports/batchGet
#' 
#' @docType package
#' @name googleAnalyticsR
NULL

options("googleAuthR.scopes.selected" = getOption("googleAnalyticsR.scope") )
options("googleAuthR.client_id" = getOption("googleAnalyticsR.client_id"))
options("googleAuthR.client_secret" = getOption("googleAnalyticsR.client_secret"))
options("googleAuthR.webapp.client_id" = getOption("googleAnalyticsR.webapp.client_id"))
options("googleAuthR.webapp.client_secret" = getOption("googleAnalyticsR.webapp.client_secret"))