#' pivot data parsing
#'
#' @keywords internal
#' @family pivot functions
pivot_ga4_parse <- function(x, hasDateComparison){
  
  if(is.null(x$columnHeader$metricHeader$pivotHeaders)) {
    return(NULL)
  }
  
  myMessage("Adding pivot data", level = 2)
  
  pivotHeaders <- x$columnHeader$metricHeader$pivotHeaders
  pivot_col_names <- makePivotNames(pivotHeaders)
  
  pivotDataRows <- x$data$rows

  # save(x, file = "pivot_debug.rda")
  
  pivotData1 <- makePivotData(pivotDataRows, pivot_col_names)
  
  if(hasDateComparison) {
    pivotData2 <- makePivotData(pivotDataRows, pivot_col_names, index=2)
    out <- cbind(pivotData1, pivotData2)
  } else {
    out <- pivotData1
  }
  
  out
  
}

#' Make pivot data rows
#'
#' @param pivotRows from x$data$rows
#' @param index 1 for normal data, 2 for date comparison
#'
#' @keywords internal
#' @family pivot functions
makePivotData <- function(pivotRows, pivotNames, index=1){
  
  if(index==2) pivotNames <- paste0(pivotNames, ".d2")
  
  pivotData <- lapply(pivotRows, function(row) row$metrics[[index]]$pivotValueRegions)
  pivotData <- lapply(lapply(pivotData, function(x) lapply(x, unlist)), unlist)
  
  pivotData <- Reduce(rbind, pivotData)
  pivotData <- matrix(as.numeric(pivotData), ncol=length(pivotNames))
  colnames(pivotData) <- pivotNames
  
  # pivotData
  
  data.frame(pivotData,
             row.names = 1:(nrow(pivotData)),
             stringsAsFactors = FALSE)
}

#' Make pivot column names
#'
#' @keywords internal
#' @family pivot functions
makePivotNames <- function(pivotHeaders){
  n <- lapply(pivotHeaders,
              ## loop over number of pivot columns expected
              function(pivots)
                Reduce(c,
                       lapply(pivots$pivotHeaderEntries,
                              ## alternating per metric
                              function(pivotCol)
                                gsub("ga:","",
                                     paste(paste(pivotCol$dimensionNames,
                                                 pivotCol$dimensionValues,
                                                 sep=".", collapse = "_"),
                                           pivotCol$metric$name, sep=".")
                                )
                       )
                )
  )
  
  out <- unlist(n)
  
  out
}

#' Make a pivot object
#'
#' @param pivot_dim A character vector of dimensions
#' @param metrics Metrics to aggregate and return.
#' @param dim_filter_clause Only data included in filter included.
#' @param startGroup which groups of k columns are included in response.
#' @param maxGroupCount Maximum number of groups to return.
#'
#' @details If maxGroupCount is set to -1 returns all groups.
#'
#' @return pivot object of class \code{pivot_ga4} for use in \code{\link{filter_clause_ga4}}
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
#' ## filter pivot results to 
#' pivot_dim_filter1 <- dim_filter("medium",
#'                                 "REGEXP",
#'                                 "organic|social|email|cpc")
#'                                 
#'                                 
#' pivot_dim_clause <- filter_clause_ga4(list(pivot_dim_filter1))
#' 
#' pivme <- pivot_ga4("medium",
#'                    metrics = c("sessions"), 
#'                    maxGroupCount = 4, 
#'                   dim_filter_clause = pivot_dim_clause)
#'                   
#'                   
#' pivtest <- google_analytics_4(ga_id, 
#'                              c("2016-01-30","2016-10-01"), 
#'                             dimensions=c('source'), 
#'                             metrics = c('sessions'), 
#'                             pivots = list(pivme))
#' 
#' }
#' 
#'
#' @export
pivot_ga4 <- function(pivot_dim, metrics, dim_filter_clause=NULL,
                      startGroup = 1, maxGroupCount = 5){
  
  assertthat::assert_that(is.character(pivot_dim),
                          is.character(metrics))
  
  expect_null_or_s3_class(dim_filter_clause, "dim_fil_ga4")
  
  dim_list <- dimension_ga4(pivot_dim)
  met_list <- metric_ga4(metrics)
  
  structure(
    list(
      dimensions = dim_list,
      dimensionFilterClauses = dim_filter_clause,
      metrics = met_list,
      startGroup = startGroup,
      maxGroupCount = maxGroupCount
    ),
    class = "pivot_ga4"
    
  )
  
  
}


