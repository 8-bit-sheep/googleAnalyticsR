#' User Activity Request
#' 
#' @param viewId The viewId
#' @param id The user or clientId.  You can send in a vector of them
#' @param id_type Whether its userId or clientId
#' @param date_range A vector of start and end dates.  If not used will default to a week.
#' 
#' 
#' @export
#' @importFrom googleAuthR gar_api_generator gar_api_page
#' @import assertthat
#' @examples 
#' 
#' \dontrun{
#' 
#' googleAuthR::gar_set_client(scopes = "https://www.googleapis.com/auth/analytics")
#' ga_auth("test.oauth")
#' ga_clientid_activity(c("1106980347.1461227730", "476443645.1541099566"),
#'                      viewId = 81416156, 
#'                      date_range = c("2019-01-01","2019-02-01"))
#' 
#' }
#' @seealso https://developers.google.com/analytics/trusted-testing/user-reporting/
#' @importFrom purrr map
ga_clientid_activity <- function(ids, 
                                 viewId, 
                                 id_type = c("CLIENT_ID","USER_ID"), 
                                 activity_type = NULL,
                                 date_range = NULL){
  ids <- as.character(ids)
  viewId <- as.character(viewId)
  id_type <- match.arg(id_type)
  
  results <- map(ids, 
                 ga_clientid_activity_one,
                 viewId = viewId,
                 id_type = id_type,
                 activity_type = activity_type,
                 date_range = date_range)
  
  list(
    users = map_dfr(results, "user"),
    sessions = map_dfr(results, "session"),
    hits = map_dfr(results, "hits")
  )
}




#' @noRd
#' @importFrom purrr map_dfr
ga_clientid_activity_one <- function(id, 
                                     viewId, 
                                     id_type, 
                                     activity_type,
                                     date_range){
  
  the_dates <- NULL
  if(!is.null(date_range)){
    date_range <- as.character(date_range)
    assert_that(length(date_range) == 2)
    the_dates <- list(
      startDate = date_range[[1]],
      endDate = date_range[[2]]
    )
  }
  
  at <- NULL
  if(!is.null(activity_type)){
    assert_that(
      is.character(activity_type),
      all(activity_type %in% c("PAGEVIEW","SCREENVIEW","GOAL","ECOMMERCE","EVENT"))
    )
    at <- activity_type
  }
  
  body <- list(
    viewId = viewId,
    user = list(
      type = id_type,
      userId = id
    ),
    activityTypes = activity_type,
    dateRange = the_dates,
    pageToken = ""
  )
  
  body <- rmNullObs(body)
  
  u <- gar_api_generator("https://analyticsreporting.googleapis.com/v4/userActivity:search",
                         "POST",
                         data_parse_function = parse_user_activity,
                         simplifyVector = FALSE)
  
  o <- gar_api_page(u,
                    page_f = page_user_activity,
                    page_method = "body",
                    page_arg = "pageToken",
                    body_list = body)
  

  list(user = map_dfr(o, "user"),
       session = map_dfr(o, "session"),
       hits = map_dfr(o, "hits"))
  
}

page_user_activity <- function(x){
  attr(x, "nextPageToken")
}



#' @noRd
#' @importFrom purrr map map_chr 
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble enframe as_tibble
#' @importFrom tidyr unnest
parse_user_activity <- function(x){
  
  o <- x
  o_summ <- as.data.frame(x$summary, stringsAsFactors = FALSE)
  o_dates <- map(x$dateGroups, "sessions")
  o_dates <- setNames(o_dates, map_chr(x$dateGroups, "activityDate"))
  o_sess <- map(o_dates, ~setNames(.x, map_chr(.x, "sessionId")))
  
  o_acts <- map(o_sess, 
                function(date){
                  map(date, 
                      function(sid){ 
                        setNames(sid$activities, 
                                 map_chr(sid$activities, 
                                         ~format(timestamp_to_r(.x[["activityTime"]]))))
                        }
                      )
                  })
  
  session_level <- setNames(map(x$dateGroups, function(x){
    setNames(map(x$sessions, function(y){
      data.frame(sessionId = y$sessionId, 
                 deviceCategory = y$deviceCategory, 
                 platform = y$platform, 
                 dataSource = y$dataSource,
                 stringsAsFactors = FALSE)   
    }), map_chr(x$sessions, "sessionId"))
  }), map_chr(x$dateGroups, "activityDate"))
  
  session_df <- session_level %>% 
    map(bind_rows) %>% 
    bind_rows(.id = "date") %>% 
    mutate(clientId = o_summ$clientId[[1]])
  
  activity <- NULL
  nested_hits <- o_acts %>% 
    map(tibble::enframe, name = "sessionId", value = "activity") %>% 
    bind_rows(.id = "date") %>% 
    tidyr::unnest() %>% 
    mutate(clientId = o_summ$clientId[[1]],
           activityTime = iso8601_to_r(map_chr(activity, "activityTime")),
           source = map_chr(activity, "source"),
           medium = map_chr(activity, "medium"),
           channelGrouping = map_chr(activity, "channelGrouping"),
           campaign = map_chr(activity, "campaign"),
           keyword = map_chr(activity, "keyword"),
           hostname = map_chr(activity, "hostname"),
           landingPagePath = map_chr(activity, "landingPagePath"),
           deviceCategory = map_chr(activity, "deviceCategory"),
           devicePlatform = map_chr(activity, "devicePlatform"),
           operatingSystem = map_chr(activity, "operatingSystem"),
           activityType = map_chr(activity, "activityType"),
           pagePath = map_chr(activity, ~safe_extract(.x$pageview$pagePath)),
           pageTitle = map_chr(activity, ~safe_extract(.x$pageview$pageTitle)),
           screenName = map_chr(activity, ~safe_extract(.x$appview$screenName)),
           mobileDeviceBranding = map_chr(activity, ~safe_extract(.x$appview$mobileDeviceBranding)),           
           mobileDeviceModel = map_chr(activity, ~safe_extract(.x$appview$mobileDeviceModel)),
           appName = map_chr(activity, ~safe_extract(.x$appview$appName)),   
           eventCategory = map_chr(activity, ~safe_extract(.x$event$eventCategory)),
           eventAction = map_chr(activity, ~safe_extract(.x$event$eventAction)),
           eventLabel = map_chr(activity, ~safe_extract(.x$event$eventLabel)),
           eventValue = map_chr(activity, ~safe_extract(.x$event$eventValue)),
           eventCount = map_chr(activity, ~safe_extract(.x$event$eventCount))
           )

  o <- list(user = as_tibble(o_summ),
            session = as_tibble(session_df),
            hits = nested_hits
            )
  attr(o, "nextPageToken") <- x$nextPageToken
  attr(o, "totalRows") <- x$totalRows
  
  o
  
}

parse_activity_row <- function(x){
  activity_type <- x$activityType
  stopifnot(activity_type %in% c("PAGEVIEW",
                                 "SCREENVIEW",
                                 "GOAL",
                                 "ECOMMERCE",
                                 "EVENT"))
  o <- x
  if(!is.null(o$customDimension)){
    o$customDimensionIndex <- o$customDimension$index
    o$customDimensionValue <- o$customDimension$value
  } else {
    o$customDimensionIndex <- NA_integer_
    o$customDimensionValue <- NA_character_
  }
  
  # init all possible values
  o$pagePath <- NA_character_
  o$pageTitle <- NA_character_
  
  o$screenName <- NA_character_
  o$mobileDeviceBranding <- NA_character_
  o$mobileDeviceModel <- NA_character_
  o$appName <- NA_character_
  
  # ecommerce TBD
  
  o$goalIndex <- 
  
  if(!is.null(x$pageview)){
    o$pagePath <- x$pageview$pagePath
    o$pageTitle <- x$pageview$pageTitle
  }  

  
 o
  
}

