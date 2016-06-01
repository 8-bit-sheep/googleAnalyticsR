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
#' 
#' @return data.frame of results
#' 
#' @seealso \url{https://support.google.com/analytics/answer/4419694?hl=en}
#' 
#' @export
google_analytics_bq <- function(projectId,
                                datasetId,
                                start=NULL,
                                end=NULL,
                                metrics = c('sessions', 'users'),
                                dimensions=NULL,
                                sort=NULL,
                                filters=NULL,
                                # segment=NULL,
                                max_results=100,
                                query=NULL){
  
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
    } else if("date" %in% dimensions){
      order_q <- paste("ORDER BY date ASC")
    } else {
      order_q <- NULL
    }
    
    query <- paste(select_q, from_q, group_q, order_q)
  }
  
  if(max_results < 50000){
    limit_q <- paste("LIMIT", as.character(max_results))
    out <- bigQueryR::bqr_query(projectId, datasetId, query)
  } else {
    ## do an async query
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
                       users = "COUNT(fullVisitorId) as users")

# lookup_bq_dimensions <- meta[meta$type == "DIMENSION","name"]

lookup_bq_query_d <- c(referralPath = "trafficSource.referralPath as referralPath",
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
                       metro = "geoNetwork.region as metro"
                       )
