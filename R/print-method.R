#' @export
print.gar_FilterExpression <- function(x, ...){

  cat0("--GA4 Filter: ", x$filter)
  cat0("==andGroup: ", x$andGroup$expressions)
  cat0("==orGroup: ", x$orGroup$expressions)
  cat0("==notExpression: ", x$notExpression)

}

#' @export
print.gar_Filter <- function(x, ...){
  cat0("--|", x$fieldName)
  cat0("----numericFilter: ", x$numericFilter)
  cat0("----inListFilter: ", x$inListFilter)
  cat0("----nullFilter: ", x$nullFilter)
  cat0("----stringFilter: ", x$stringFilter)
  cat0("----betweenFilter: ", x$betweenFilter)
}

#' @export
print.gar_NumericFilter <- function(x, ...){
  cat("operation: ", x$operation, 
      "| value:     ", x$value[[1]])
}

#' @export
print.gar_InListFilter <- function(x, ...){
  cat0("values: ", paste(x$values, collapse = " "))
  cat0("caseSensitive: ", x$caseSensitive)
}

#' @export
print.gar_NullFilter <- function(x, ...){
  cat("NULL")
}

#' @export
print.gar_StringFilter <- function(x, ...){
  cat("value: ", x$value[[1]], 
      "| matchType: ", x$matchType, 
      "| caseSensitive: ", x$caseSensitive)
}

#' @export
print.gar_BetweenFilter <- function(x, ...){
  cat("from: ", x$fromValue[[1]], " to: ", x$toValue[[1]])
}



#' @export
print.ga_model_result <- function(x, ...){
  cat("==ga_model_result object==\n")
  cat0("Input names:       ", paste(names(x$input), collapse = " "))
  cat0("Input dimensions:  ", dim(x$input))
  cat0("Output names:      ", paste(names(x$output), collapse = " "))
  cat0("Output dimensions: ", dim(x$output))
  cat0("Plot class:        ", class(x$plot))
  if(!is.null(x$plot)) print(x$plot)
  cat0("Model args passed: ", paste(names(x$args),"=", x$args))
  print(x$model)
}




#' @export
print.ga_model <- function(x, ...){
  cat("==ga_model object==\n")
  cat0("Description: ", x$description)
  cat0("Data args:   ", paste(function_args(x$data_f), collapse = " "))
  cat0("Input data:  ", paste(x$required_columns, collapse = " "))
  cat0("Model args:  ", paste(function_args(x$model_f), collapse = " "))
  if(!is.null(x$output_f)) cat0("Output args: ", paste(function_args(x$output_f), collapse = " "))
  cat0("Packages:    ", paste(x$required_packages, collapse = " "))
}


# #' @export
# print.ga_profileFilterLink <- function(x, ...){
#   cat("==Google Analytics Profile (View) Filter Link==\n")
#   cat0("profileFilterLink ID:  ", x$id)
#   cat0("Rank:                  ", x$rank)
#   cat0("Web Property Id:       ", x$webPropertyId)
#   cat0("Upload ID:             ", x$id)
#   cat0("Status:                ", x$status)
#   
#   
# }

#' @export
print.customDimension_ga <- function(x, ...){
  cat("==Google Analytics Custom Dimension==\n")
  cat0("Name:         ", x$name)
  cat0("Id:           ", x$id)
  cat0("Index:        ", x$index)
  cat0("Scope:        ", x$scope)
  cat0("Active:       ", x$active)
  cat0("Created:      ", x$created)
  cat0("Updated:      ", x$updated)
  cat0("AccountId:    ", x$accountId)
  cat0("WebPropertyId:", x$webPropertyId)
}


#' @export
print.ga_custom_data_source_upload <- function(x, ...){
  cat("==Google Analytics Custom Data Source Upload==\n")
  cat0("Custom Data Source ID: ", x$customDataSourceId)
  cat0("Account ID:            ", x$accountId)
  cat0("Web Property Id:       ", x$webPropertyId)
  cat0("Upload ID:             ", x$id)
  cat0("Status:                ", x$status)
}

#' @export
print.ga_custom_datasource <- function(x, ...){
  y <- attr(x, "meta")
  
  out <- data.frame(name = x$name,
                    type = x$type,
                    id = x$id,
                    accountId = x$accountId,
                    webPropertyId = x$webPropertyId,
                    importBehavior = if(!is.null(x$importBehavior)) x$importBehavior else NULL,
                    created = x$created,
                    updated = x$updated,
                    stringsAsFactors = FALSE)
  
  if(!is.null(out$created)) out$created <- iso8601_to_r(out$created)
  if(!is.null(out$updated)) out$updated <- iso8601_to_r(out$updated)
  
  cat("==Google Analytics Custom Data Sources==\n")
  cat0("Username:            ", y$username)
  cat0("Total Results:       ", y$totalResults)
  cat("=Datasource list=\n")
  print.data.frame(out)
  cat("=Profiles linked=\n")
  cat0("- ", unlist(x$profilesLinked))
  
}