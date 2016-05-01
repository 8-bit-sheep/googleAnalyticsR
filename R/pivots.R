#' pivot data parsing
#'
#' @keywords internal
#' @family pivot functions
pivot_ga4_parse <- function(x, hasDateComparison){
  if(is.null(x$columnHeader$metricHeader$pivotHeaders)) {
    return(NULL)
  }
  
  message("Adding pivot data")
  
  pivotHeaders <- x$columnHeader$metricHeader$pivotHeaders
  pivot_col_names <- makePivotNames(pivotHeaders)
  
  pivotDataRows <- x$data$rows
  
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
  
  pivotData <- lapply(pivotRows, function(row) row$metrics[[index]]$pivotValues)
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
  
  pivNames <- lapply(n, make.names, unique = TRUE)
  
  unlist(pivNames)
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
#' @export
pivot_ga4 <- function(pivot_dim, metrics, dim_filter_clause=NULL,
                      startGroup = 1, maxGroupCount = 5){
  
  stopifnot(inherits(pivot_dim, "character"),
            inherits(metrics, "character"),
            any(is.null(dim_filter_clause), inherits(dim_filter_clause, "list")))
  
  dim_list <- dimension_ga4(pivot_dim)
  met_list <- metric_ga4(metrics)
  
  if(!is.null(dim_filter_clause)) stopifnot(inherits(dim_filter_clause, "dim_fil_ga4"))
  
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


