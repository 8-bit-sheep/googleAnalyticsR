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
  
  if(!is.null(out$created)) out$created <- timestamp_to_r(out$created)
  if(!is.null(out$updated)) out$updated <- timestamp_to_r(out$updated)
  
  cat("==Google Analytics Custom Data Sources==\n")
  cat0("Username:            ", y$username)
  cat0("Total Results:       ", y$totalResults)
  cat("=Datasource list=\n")
  print.data.frame(out)
  cat("=Profiles linked=\n")
  cat0("- ", unlist(x$profilesLinked))
  
}