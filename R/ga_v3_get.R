#' Get Google Analytics v3 data
#'
#' @param id A character vector of View Ids to fetch from.
#' @param start Start date in YYY-MM-DD format.
#' @param end End date in YYY-MM-DD format.
#' @param metrics A character vector of metrics. With or without ga: prefix.
#' @param dimensions A character vector of dimensions. With or without ga: prefix.
#' @param sort How to sort the results, in form 'ga:sessions,-ga:bounceRate'
#' @param filters Filters for the result, in form 'ga:sessions>0;ga:pagePath=~blah'
#' @param segment How to segment.
#' @param samplingLevel Choose "WALK" to mitigate against sampling.
#' @param max_results Default 100. If greater than 10,000 then will batch GA calls.
#' @param multi_account_batching If TRUE then multiple id's are fetched together.  
#'     Not compatible with \code{samplingLevel="WALK"} or \code{max_results>10000}
#' @param type ga = Google Analytics v3; mcf = Multi-Channel Funels.
#'
#' @return For one id a data.frame of data, with meta-data in attributes.  
#'   For multiple id's, a list of dataframes.
#' 
#' @seealso https://developers.google.com/analytics/devguides/reporting/core/v3/
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' library(googleAnalyticsR)
#' 
#' ## Authenticate in Google OAuth2
#' ## this also sets options
#' ga_auth()
#' 
#' ## if you need to re-authenticate use ga_auth(new_user=TRUE)
#' ## if you have your own Google Dev console project keys,
#' ## then don't run ga_auth() as that will set to the defaults.
#' ## instead put your options here, and run googleAuthR::gar_auth()
#' 
#' ## get account info, including View Ids
#' account_list <- google_analytics_account_list()
#' ga_id <- account_list$viewId[1]
#' 
#' ## get a list of what metrics and dimensions you can use
#' 
#' meta <- google_analytics_meta()
#' head(meta)
#' 
#' ## pick the account_list$viewId you want to see data for.
#' ## metrics and dimensions can have or have not "ga:" prefix
#' 
#' gadata <- google_analytics(id = ga_id,
#'                            start="2015-08-01", end="2015-08-02",
#'                            metrics = c("sessions", "bounceRate"),
#'                            dimensions = c("source", "medium"))
#'                        
#'  ## multi accounts, pass character vector of viewIds
#'  ## outputs a list of data.frames, named after the viewId
#'  multi_gadata <- google_analytics(id = c("123456","9876545","765432"),
#'                                   start="2015-08-01", end="2015-08-02",
#'                                   metrics = c("sessions", "bounceRate"),
#'                                    dimensions = c("source", "medium"))
#'                                 
#' ## if more than 10000 rows in results, auto batching
#' ## example is setting lots of dimensions to try and create big sampled data
#' batch_gadata <- google_analytics(id = ga_id,
#'                                  start="2014-08-01", end="2015-08-02",
#'                                  metrics = c("sessions", "bounceRate"),
#'                                  dimensions = c("source", "medium", 
#'                                                "landingPagePath",
#'                                                "hour","minute"),
#'                                 max=99999999)
#' 
#' ## mitigate sampling by setting samplingLevel="WALK"
#' ## this will send lots and lots of calls to the Google API limits, beware
#' walk_gadata <- google_analytics(id = ga_id,
#'                                 start="2014-08-01", end="2015-08-02",
#'                                 metrics = c("sessions", "bounceRate"),
#'                                 dimensions = c("source", "medium", "landingPagePath"),
#'                                 max=99999999, samplingLevel="WALK")
#'                                 
#' ## multi-channel funnels set type="mcf"
#' mcf_gadata <- google_analytics(id = ga_id,
#'                                start="2015-08-01", end="2015-08-02",
#'                                metrics = c("totalConversions"),
#'                                dimensions = c("sourcePath"),
#'                                type="mcf")
#' 
#' 
#' ## reach meta-data via attr()
#' attr(gadata, "profileInfo")
#' attr(gadata, "dateRange")
#' 
#' 
#' }
#' 
#' @importFrom googleAuthR gar_api_generator
#' 
#' @export
google_analytics <- function(id,
                             start,
                             end,
                             metrics = c('sessions', 'bounceRate'),
                             dimensions=NULL,
                             sort=NULL,
                             filters=NULL,
                             segment=NULL,
                             samplingLevel=c("DEFAULT", 
                                             "FASTER",
                                             "HIGHER_PRECISION", 
                                             "WALK"),
                             max_results=100,
                             multi_account_batching = FALSE,
                             type = c("ga", "mcf")) {
  
  samplingLevel <- match.arg(samplingLevel)
  start <- as.character(start)
  end <- as.character(end)
  type <-  match.arg(type)
  
  if(multi_account_batching){
    if(max_results>10000){
      warning("Can't use multi_account_batching=TRUE when max_results>10000. 
              Setting back to FALSE")
      multi_account_batching <- FALSE
    }
    if(samplingLevel=="WALK"){
      warning("Can't use multi_account_batching=TRUE when samplingLevel=='WALK'. 
              Setting back to FALSE")
      multi_account_batching <- FALSE 
    }
    if(length(id) == 1){
      warning("multi_account_batching=TRUE but only one id specified. 
              Ignoring.")
      multi_account_batching <- FALSE 
    }
  }
  
  if(!is.null(filters)) {
    filters <- utils::URLencode(filters, reserved = TRUE)
  } 
  
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
                  output="json")
  
  ga_pars <- rmNullObs(ga_pars)
  
  # all_data <- list()
  ga <- gar_api_generator(paste0("https://www.googleapis.com/analytics/v3/data/",
                                 type),
                          "GET",
                          pars_args = ga_pars,
                          data_parse_function = parse_google_analytics)
  
  if(multi_account_batching){
    myMessage("Fetching all ids at same time (max 10 per API call)", level = 3)
    all_data <- googleAuthR::gar_batch_walk(ga,
                                            walk_vector = id,
                                            gar_pars = ga_pars,
                                            pars_walk = 'ids',
                                            data_frame_output = FALSE,
                                            batch_size = 10)
    all_data <- unlist(all_data, recursive = FALSE, use.names = FALSE)
    names(all_data) <- id
    
  } else {
    all_data <- loop_ids(id=id, 
                         ga_pars=ga_pars, 
                         samplingLevel=samplingLevel, 
                         max_results=max_results, 
                         ga=ga)
  }

  
  all_data
  
}



