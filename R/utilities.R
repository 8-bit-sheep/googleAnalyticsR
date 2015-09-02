#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

#' check it starts with ga: and if not puts it on
#'
#' @keywords internal
checkPrefix <- function(x, prefix=c("ga", "mcf")){
  prefix <- match.arg(prefix)

  prefix_reg <- paste0("^",prefix,":")

  if(grepl(prefix_reg, x)) x else paste0(prefix,":",x)
}

#' Add name of list entry of dataframe to dataframe colum
#'
#' @keywords internal
listNameToDFCol <- function(named_list, colName = "listName"){
  lapply(names(named_list),
                function(x) {named_list[[x]][colName] <- x
                named_list[[x]]
                })
}

#' Is this a try error?
#'
#' Utility to test errors
#'
#' @param test_me an object created with try()
#'
#' @return Boolean
#'
#' @keywords internal
is.error <- function(test_me){
  inherits(test_me, "try-error")
}

#' Get the error message
#'
#' @param test_me an object that has failed is.error
#'
#' @return The error message
#'
#' @keywords internal
error.message <- function(test_me){
  if(is.error(test_me)) attr(test_me, "condition")$message
}


batchData <- function(ga, ga_pars){

  ga_pars$`max-results` <- 10000
  the_data <- ga(pars_arguments = ga_pars)
  batches <- attr(the_data, "totalResults") %/% 10000

  anyBatchSampled <- attr(the_data, 'containsSampledData')
  samplePercent   <- attr(the_data, "samplePercent")

  if(batches > 0){
    for(i in 1:batches){
      message("Batching data [", i+1, "] of [", batches+1, "]")
      ga_pars$`start-index` <- 1 + (i*10000)
      batch_data <- ga(pars_arguments = ga_pars)

      if(attr(batch_data, 'containsSampledData')) anyBatchSampled <- TRUE
      samplePercent <- samplePercent + attr(batch_data, "samplePercent")

      message("samplePercent:", samplePercent)
      message("batches:", batches)
      message("samplePercent / batches:", samplePercent / batches)

      the_data <- rbind(the_data, batch_data)

    }
  }

  message("All data found. [",
          NROW(the_data), "] total results out of a possible [",
          attr(the_data, "totalResults") , "]" )

  attr(the_data, "containsSampledData") <- anyBatchSampled
  attr(the_data, "samplePercent") <- samplePercent / (batches + 1)
  the_data

}

## now in googleAuthR
# retryFetchData <- function(f){
#   the_data <- try(f)
#   if(is.error(the_data)){
#     message("Failed to fetch data: ", error.message(the_data))
#     ## insert conditions for retry so it doesn't do it for all errors
#     ## if(error.message(the_data) %in% c('userRateLimitExceeded', 'quotaExceeded', 'internalServerError', 'backendError'))
#     if(grepl('userRateLimitExceeded|quotaExceeded|internalServerError|backendError', error.message(the_data))){
#       for(i in 1:getOption("googleAnalyticsR.tryAttempts")){
#         message("Trying again: ", i, " of ", getOption("googleAnalyticsR.tryAttempts"))
#         Sys.sleep((2 ^ i) + runif(n = 1, min = 0, max = 1))
#         the_data <- try(f)
#         if(!is.error(the_data)) break
#       }
#       warning("All attempts failed.")
#     } else {
#       stop(error.message(the_data))
#     }
#
#   }
#
#   the_data
# }

walkData <- function(ga, ga_pars, start, end){
  dates <- as.character(
    seq(as.Date(start, format="%Y-%m-%d"),
        as.Date(end, format="%Y-%m-%d"),
        by=1))

  ga_pars$`start-date` <- dates[1]
  ga_pars$`end-date`   <- dates[1]
  ga_pars$samplingLevel <- "HIGHER_PRECISION"

  walked_data <- batchData(ga, ga_pars)

  anyWalksSampled <- attr(walked_data, "containsSampledData")
  samplePercent   <- attr(walked_data, "samplePercent")

    for(d in dates[-1]){
    message("Walking data for ", d)
    ga_pars$`start-date` <- d
    ga_pars$`end-date`   <- d

    day_data <- batchData(ga, ga_pars)

    if(NROW(day_data) > 0){

      if(attr(day_data, 'containsSampledData')) anyWalksSampled <- TRUE
      samplePercent <- samplePercent + attr(day_data, "samplePercent")

      walked_data <- rbind(walked_data, day_data, make.row.names=F)

    } else {
      message("No data for ", d)
    }
  }

  message("Walked through all dates. Total Results: [", NROW(walked_data), "]")
  attr(walked_data, "dateRange") <- list(startDate = start, endDate = end)
  attr(walked_data, "totalResults") <- NROW(walked_data)
  attr(walked_data, "samplingLevel") <- "HIGHER_PRECISION, WALKED"
  attr(walked_data, "containsSampledData") <- anyWalksSampled
  attr(walked_data, "samplePercent") <- samplePercent / length(dates)

  walked_data

}


