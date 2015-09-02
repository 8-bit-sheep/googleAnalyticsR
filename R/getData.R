options("googleAuthR.scopes.selected" = getOption("googleAnalyticsR.scope") )
options("googleAuthR.client_id" = getOption("googleAnalyticsR.client_id"))
options("googleAuthR.client_secret" = getOption("googleAnalyticsR.client_secret"))
options("googleAuthR.webapp.client_id" = getOption("googleAnalyticsR.webapp.client_id"))
options("googleAuthR.webapp.client_secret" = getOption("googleAnalyticsR.webapp.client_secret"))


#' Get Google Analytics data
#'
#' @param id A character vector of View Ids to fetch from.
#' @param start Start date in YYY-MM-DD format.
#' @param end End date in YYY-MM-DD format.
#' @param metrics A character vector of metrics. With or without ga: prefix.
#' @param dimensions A character vector of dimensions. With or without ga: prefix.
#' @param sort How to sort the results, in form 'ga:sessions,-ga:bounceRate'
#' @param filters Filters for the result, in form 'ga:sessions>0;ga:pagePath=~blah'
#' @param segment How to segment
#' @param samplingLevel. Choose "WALK" to do an API call per day to mitigate against sampling.
#' @param max_results Select -1 for all data. If greater than 10,000 then will batch GA calls.
#' @param type ga = google Analytics; mcf = Multi-Channel Funels.
#'
#' @return For one id a data.frame of data, with meta-data in attributes.  For multiple id, a list of dataframes.
#' @export
#'
#' @seealso https://developers.google.com/analytics/devguides/reporting/core/v3/
google_analytics <- function(id,
                             start,
                             end,
                             metrics = c('sessions', 'bounceRate'),
                             dimensions=NULL,
                             sort=NULL,
                             filters=NULL,
                             segment=NULL,
                             samplingLevel=c("DEFAULT", "FASTER","HIGHER_PRECISION", "WALK"),
                             max_results=-1,
                             type = c("ga", "mcf")) {

  samplingLevel <- match.arg(samplingLevel)
  start <- as.character(start)
  end <- as.character(end)
  type <-  match.arg(type)

  id <- sapply(id, checkPrefix, prefix = "ga")
  metrics <- paste(sapply(metrics, checkPrefix, prefix=type), collapse=",")
  dimensions <- paste(sapply(dimensions, checkPrefix, prefix=type), collapse=",")


  ga_pars <- list(ids = id[1],
                  "start-date"=start,
                  "end-date"=end,
                  metrics=metrics,
                  dimensions=dimensions,
                  sort=sort,
                  filters=filters,
                  segment=segment,
                  samplingLevel=samplingLevel,
                  "start-index"=1,
                  "max-results"=max_results,
                  "start-index"=1,
                  output="json")

  # ga_pars <- rmNullObs(ga_pars)

  all_data <- list()
  for(i in id){
    ga_pars$ids <- i

    ga <-
      googleAuthR::gar_api_generator(paste0("https://www.googleapis.com/analytics/v3/data/",type),
                                     "GET",
                                     pars_args = ga_pars,
                                     data_parse_function = parse_google_analytics)

    ## if walk through results then split up dates and walk through date ranges
    if(samplingLevel %in% "WALK"){
      message("Walking through data.")
      the_data <- walkData(ga, ga_pars, start, end)

    } else {
      ## No batch needed
      if(all(max_results < 10000, max_results > 0)) {
        the_data <- ga(pars_arguments = ga_pars)
      } else {
        ## batching data
        the_data <- batchData(ga, ga_pars)
      }

    }

    if(length(id) > 1){
      ## for multiple id's, a list of dataframes.
      id_name <- attr(the_data, "profileInfo")$profileId

      all_data[[id_name]] <- the_data

    } else {
      ## for one id, just a dataframe
      all_data <- the_data

    }

  }

  all_data

}

#' Get current dimensions and metrics available in GA API.
#'
#' @return dataframe of dimensions and metrics available to use
#'
#'  @seealso https://developers.google.com/analytics/devguides/reporting/metadata/v3/reference/metadata/columns/list
#'
google_analytics_meta <- function(){

  meta <-
    googleAuthR::gar_api_generator("https://www.googleapis.com/analytics/v3",
                                   "GET",
                                   path_args = list(metadata = "ga",
                                                    columns = ""),
                                   data_parse_function = parse_google_analytics_meta )

  meta()

}

#' Get account list data including web property and View Id
#'
#' @seealso https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/management/accountSummaries/list
#'
#' @return a dataframe of all account, webproperty and view data
#'
#' @export
google_analytics_account_list <- function(){

  acc_sum <- googleAuthR::gar_api_generator("https://www.googleapis.com/analytics/v3/management/accountSummaries",
                                            "GET",
                                            data_parse_function = parse_ga_account_summary)
  acc_sum()

}
