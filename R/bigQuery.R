#' google_analytics_bq
#' 
#' Turn a google_analytics style call into BigQuery SQL. 
#' Used with Google Analytics 360 BigQuery exports.
#' 
#' All data will be unsampled, and requests will cost money against your BigQuery quota.
#' 
#' Requires installation of bigQueryR and authentication under ga_bq_auth() 
#' or googleAuthR::gar_auth() with BigQuery scope set.
#' 
#' No segments for now
#' 
#' View your projectIds upon authentication via \code{\link[bigQueryR]{bqe_list_projects}}:
#' 
#' @param projectId The Google project Id where the BigQuery exports sit.
#' @param datasetId DatasetId of GA export.  This should match the GA View ID.
#' @param start start date
#' @param end end date
#' @param metrics metrics to query
#' @param dimensions dimensions to query
#' @param sort metric to sort by
#' @param filters filter results
#' @param max_results How many results to fetch
#' @param query If query is non-NULL then it will use that and ignore above
#' @param return_query_only Only return the constructed query, don't call BigQuery.
#' 
#' @return data.frame of results
#' 
#' @seealso \url{https://support.google.com/analytics/answer/4419694?hl=en}
#'          \url{https://support.google.com/analytics/answer/3437719?hl=en}
#' 
#' @export
google_analytics_bq <- function(projectId,
                                datasetId,
                                start=NULL,
                                end=NULL,
                                metrics = NULL,
                                dimensions=NULL,
                                sort=NULL,
                                filters=NULL,
                                # segment=NULL,
                                max_results=100,
                                query=NULL,
                                return_query_only=FALSE){
  
  projectId <- as.character(projectId)
  datasetID <- as.character(datasetId)
  start <- if(!is.null(start)) as.character(as.Date(start))
  end <- if(!is.null(end)) as.character(as.Date(end))

  
  if (!requireNamespace("bigQueryR", quietly = TRUE)) {
    stop("bigQueryR needed for this function to work. Please install it via devtools::install_github('MarkEdmondson1234/bigQueryR')",
         call. = FALSE)
  }
  
  # bq_tables <- bigQueryR::bqr_list_tables(projectId, datasetId)
  
  ## if Sys.Date() == end then construct for ga_sessions_intradata_ too.
  if(is.null(query)){
    
    if(any(is.na(lookup_bq_query_m[metrics]))){
      stop("metric not yet supported. Must be one of ", paste(names(lookup_bq_query_m), collapse = ", "))
    }
    
    if(any(is.na(lookup_bq_query_d[dimensions]))){
      stop("dimension not yet supported. Must be one of ", paste(names(lookup_bq_query_d), collapse = ", "))
    }
    
    if(as.Date(end) == Sys.Date()){
      from_q <- sprintf("FROM (TABLE_DATE_RANGE([%s.ga_sessions_], TIMESTAMP('%s'), DATE_ADD(CURRENT_TIMESTAMP(), -1, 'DAY'))), (TABLE_DATE_RANGE([%s.ga_sessions_intraday_], DATE_ADD(CURRENT_TIMESTAMP(), -1, 'DAY'), CURRENT_TIMESTAMP()))", 
                        datasetId, start, datasetId)
    } else {
      from_q <- sprintf("FROM (TABLE_DATE_RANGE([%s.ga_sessions_], TIMESTAMP('%s'), TIMESTAMP('%s')))", 
                        datasetId, start, end)
    }
    
    if(!is.null(dimensions)){
      dims <- paste(lookup_bq_query_d[dimensions], collapse = ", ", sep = ", ")
      group_q <- paste("GROUP BY", paste(dimensions, collapse = ", "))
    } else {
      dims <- NULL
      group_q <- NULL
    }
    
    mets <- paste(lookup_bq_query_m[metrics], collapse = ", ", sep = ", ")
    select_q <- paste("SELECT", paste(dims, mets, sep = ", "))
    
    if(!is.null(sort)){
      order_q <- paste("ORDER BY", paste(sort, collapse = ", "), "DESC")
    } else {
      order_q <- NULL
    }
    
    limit_q <- paste("LIMIT", as.character(max_results))
    
    query <- paste(select_q, from_q, group_q, order_q, limit_q)
    
    if(return_query_only){
      return(query)
    } else {
      message("Query: ", query)
    }
  }
  
  if(max_results < 70000){

    out <- bigQueryR::bqr_query(projectId, datasetId, query)
  } else {
    ## do an async query
    message("Currently only up to 70,000 rows fetched per call.")
    return()
  }
  
  ## convert to more R like objects if we can
  if("hitTimestamp" %in% names(out)){
    out$hitTimestamp <- as.POSIXct(out$hitTimestamp, origin = "1970-01-01")
  }
  
  if("date" %in% names(out)){
    out$date <- as.Date(out$date, format = "%Y%m%d")
  }
  
  if("visitStartTime" %in% names(out)){
    out$visitStartTime <- as.POSIXct(out$visitStartTime, origin = "1970-01-01")
  }
  
  out
}

lookup_bq_query_m <- c(visits = "SUM(totals.visits) as sessions",
                       sessions = "SUM(totals.visits) as sessions",
                       pageviews = "SUM(totals.pageviews) as pageviews",
                       timeOnSite = "SUM(totals.timeOnSite) as timeOnSite",
                       bounces = "SUM(totals.bounces) as bounces",
                       transactions = "SUM(totals.transactions) as transactions",
                       transactionRevenue = "SUM(totals.transactionRevenue)/1000000 as transactionRevenue",
                       newVisits = "SUM(totals.newVisits) as newVisits",
                       screenviews = "SUM(totals.screenviews) as screenviews",
                       uniqueScreenviews = "SUM(totals.uniqueScreenviews) as uniqueScreenviews",
                       timeOnScreen = "SUM(totals.timeOnScreen) as timeOnScreen",
                       users = "COUNT(fullVisitorId) as users",
                       exits = "COUNT(hits.isExit) as exits",
                       entrances = "COUNT(hits.isEntrance) as entrances",
                       eventValue = "SUM(hits.eventinfo.eventValue) as eventValue")

# lookup_bq_dimensions <- meta[meta$type == "DIMENSION","name"]

lookup_bq_query_d <- c(referralPath = "trafficSource.referralPath as referralPath",
                       hitTimestamp = "(visitStartTime + (hits.time/1000)) as hitTimestamp",
                       campaign = "trafficSource.campaign as campaign",
                       source = "trafficSource.source as source",
                       medium = "trafficSource.medium as medium",
                       keyword = "trafficSource.keyword as keyword",
                       adContent = "trafficSource.adContent as adContent",
                       adwordsCampaignID = "trafficSource.adwordsClickInfo.campaignId as adwordsCampaignId",
                       adwordsAdGroupID = "trafficSource.adwordsClickInfo.adGroupId as adwordsAdGroupId",
                       # adwords...etc...
                       transactionId = "hits.transaction.transactionId as transactionId",
                       date = "date",
                       fullVisitorId = "fullVisitorId",
                       userId = "userId",
                       visitorId = "visitorId",
                       visitId = "visitId",
                       visitStartTime = "visitStartTime",
                       visitNumber = "visitNumber",
                       browser = "device.browser as browser",
                       browserVersion = "device.browserVersion as browserVersion",
                       operatingSystem = "device.operatingSystem as operatingSystem",
                       operatingSystemVersion = "device.operatingSystemVersion as operatingSystemVersion",
                       mobileDeviceBranding = "device.mobileDeviceBranding as mobileDeviceBranding",
                       flashVersion = "device.flashVersion as flashVersion",
                       language = "device.language as language",
                       screenColors = "device.screenColors as screenColors",
                       screenResolution = "device.screenResolution as screenResolution",
                       deviceCategory = "device.deviceCategory as deviceCategory",
                       continent = "geoNetwork.continent as continent",
                       subContinent = "geoNetwork.subContinent as subContinent",
                       country = "geoNetwork.country as country",
                       region = "geoNetwork.region as region",
                       metro = "geoNetwork.region as metro",
                       pagePath = "hits.page.pagePath as pagePath",
                       eventCategory = "hits.eventInfo.eventCategory as eventCategory",
                       eventAction = "hits.eventInfo.eventAction as eventAction",
                       eventLabel = "hits.eventInfo.eventLabel as eventLabel"
                       )
