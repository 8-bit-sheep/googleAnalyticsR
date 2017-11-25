#' List filters for view (profile)
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_view_list <- function(accountId,
                                webPropertyId,
                                profileId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 profiles = profileId,
                                 profileFilterLinks = ""
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}

#' Get specific filter for view (profile)
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param profileId Profile Id
#' @param linkId Link Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_view <- function(accountId,
                           webPropertyId,
                           profileId,
                           linkId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 webproperties = webPropertyId,
                                 profiles = profileId,
                                 profileFilterLinks = linkId
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}

#' Get specific filter for account
#'
#' @param accountId Account Id
#' @param filterId Filter Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter <- function(accountId,
                      filterId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 filters = filterId
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}

#' List filters for account
#'
#' @param accountId Account Id
#'
#' @return filter list
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_filter_list <- function(accountId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  filters <- gar_api_generator(url,
                               "GET",
                               path_args = list(
                                 accounts = accountId,
                                 filters = ""
                               ),
                               data_parse_function = function(x) x)
  
  filters()
  
}



#' Make a dimension or metric filter clause object
#'
#' @param filters a list of \link{dim_filter} or \link{met_filter}. Only one type allowed.
#' @param operator combination of filter.
#'
#' @details If you have dimension and metric filters,
#'   make the clauses in two separate calls, then pass the objects to \link{make_ga_4_req}
#'
#'
#' @return An object of class \code{dim_fil_ga4} or \code{met_fil_ga4} 
#'   for use in \code{\link{make_ga_4_req}}
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' 
#' ## authenticate, 
#' ## or use the RStudio Addin "Google API Auth" with analytics scopes set
#' ga_auth()
#' 
#' ## get your accounts
#' account_list <- google_analytics_account_list()
#' 
#' ## pick a profile with data to query
#' 
#' ga_id <- account_list[23,'viewId']
#' 
#' ## create filters on metrics
#' mf <- met_filter("bounces", "GREATER_THAN", 0)
#' mf2 <- met_filter("sessions", "GREATER", 2)
#' 
#' ## create filters on dimensions
#' df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
#' df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)
#' 
#' ## construct filter objects
#' fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
#' fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")
#' 
#' ## make v4 request
#' ga_data1 <- google_analytics_4(ga_id, 
#'                                date_range = c("2015-07-30","2015-10-01"),
#'                                dimensions=c('source','medium'), 
#'                                metrics = c('sessions','bounces'), 
#'                                met_filters = fc, 
#'                                dim_filters = fc2, 
#'                                filtersExpression = "ga:source!=(direct)")
#'                                
#' }
#' 
#' 
#' @export
#' @family filter functions
filter_clause_ga4 <- function(filters, operator = c("OR", "AND")){
  operator <- match.arg(operator)
  
  stopifnot(inherits(filters, "list"))
  
  ## get class names to seee if all the same
  types <- unlist(lapply(filters, class))
  
  stopifnot(all(types[1] == types), types[1] %in% c("dim_fil_ga4","met_fil_ga4"))
  class_name <- types[1]
  
  structure(
    list(
      operator = operator,
      filters = filters
    ),
    class = class_name
  )
  
}


#' Make a dimension filter object
#'
#' @param dimension dimension name to filter on.
#' @param operator How to match the dimension.
#' @param expressions What to match.  A character vector if operator is "IN_LIST"
#' @param caseSensitive Boolean.
#' @param not Logical NOT operator. Boolean.
#'
#' @return An object of class \code{dim_fil_ga4} for use in \code{\link{filter_clause_ga4}}
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' 
#' ## authenticate, 
#' ## or use the RStudio Addin "Google API Auth" with analytics scopes set
#' ga_auth()
#' 
#' ## get your accounts
#' account_list <- google_analytics_account_list()
#' 
#' ## pick a profile with data to query
#' 
#' ga_id <- account_list[23,'viewId']
#' 
#' ## create filters on metrics
#' mf <- met_filter("bounces", "GREATER_THAN", 0)
#' mf2 <- met_filter("sessions", "GREATER", 2)
#' 
#' ## create filters on dimensions
#' df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
#' df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)
#' 
#' ## construct filter objects
#' fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
#' fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")
#' 
#' ## make v4 request
#' ga_data1 <- google_analytics_4(ga_id, 
#'                                date_range = c("2015-07-30","2015-10-01"),
#'                                dimensions=c('source','medium'), 
#'                                metrics = c('sessions','bounces'), 
#'                                met_filters = fc, 
#'                                dim_filters = fc2, 
#'                                filtersExpression = "ga:source!=(direct)")
#'                                
#' }
#' 
#' @export
#' @family filter functions
dim_filter <- function(dimension,
                       operator = c("REGEXP","BEGINS_WITH","ENDS_WITH",
                                    "PARTIAL","EXACT","NUMERIC_EQUAL",
                                    "NUMERIC_GREATER_THAN","NUMERIC_LESS_THAN","IN_LIST"),
                       expressions,
                       caseSensitive = FALSE,
                       not = FALSE){
  
  operator <- match.arg(operator)
  
  stopifnot(inherits(dimension, "character"),
            inherits(expressions, "character"))
  
  dimension <- sapply(dimension, checkPrefix, prefix = "ga")
  
  if(all(operator != "IN_LIST", length(expressions) > 1)) {
    warning("Only first expression used if operator not 'IN_LIST'")
    expressions <- expressions[1]
  }
  
  structure(
    list(
      dimensionName = dimension,
      not = not,
      operator = operator,
      expressions = as.list(expressions),
      caseSensitive = caseSensitive
    ),
    class = "dim_fil_ga4"
  )
}

#' Make a metric filter object
#'
#' @param metric metric name to filter on.
#' @param operator How to match the dimension.
#' @param comparisonValue What to match.
#' @param not Logical NOT operator. Boolean.
#'
#' @return An object of class \code{met_fil_ga4} for use in \code{\link{filter_clause_ga4}}
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' 
#' ## authenticate, 
#' ## or use the RStudio Addin "Google API Auth" with analytics scopes set
#' ga_auth()
#' 
#' ## get your accounts
#' account_list <- google_analytics_account_list()
#' 
#' ## pick a profile with data to query
#' 
#' ga_id <- account_list[23,'viewId']
#' 
#' ## create filters on metrics
#' mf <- met_filter("bounces", "GREATER_THAN", 0)
#' mf2 <- met_filter("sessions", "GREATER", 2)
#' 
#' ## create filters on dimensions
#' df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
#' df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)
#' 
#' ## construct filter objects
#' fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
#' fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")
#' 
#' ## make v4 request
#' ga_data1 <- google_analytics_4(ga_id, 
#'                                date_range = c("2015-07-30","2015-10-01"),
#'                                dimensions=c('source','medium'), 
#'                                metrics = c('sessions','bounces'), 
#'                                met_filters = fc, 
#'                                dim_filters = fc2, 
#'                                filtersExpression = "ga:source!=(direct)")
#'                                
#' } 
#' 
#' @export
#' @family filter functions
met_filter <- function(metric,
                       operator = c("EQUAL","LESS_THAN","GREATER_THAN","IS_MISSING"),
                       comparisonValue,
                       not = FALSE){
  operator <- match.arg(operator)
  
  stopifnot(inherits(metric, "character"))
  
  metric <- sapply(metric, checkPrefix, prefix = "ga")
  
  structure(
    list(
      metricName = metric,
      not = not,
      operator = operator,
      comparisonValue = as.character(comparisonValue)
    ),
    class = "met_fil_ga4"
  )
  
}
