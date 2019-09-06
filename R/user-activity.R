#' User Activity Request
#' 
#' Get activity on an individual user
#' 
#' @param viewId The viewId
#' @param ids The userId or clientId.  You can send in a vector of them
#' @param id_type Whether its userId or clientId
#' @param date_range A vector of start and end dates.  If not used will default to a week.
#' @param activity_type If specified, filters down response to the activity type.  Choice between \code{"PAGEVIEW","SCREENVIEW","GOAL","ECOMMERCE","EVENT"}
#' 
#' @details 
#' 
#' The User Activity API lets you query an individual user's movement through your website, by sending in the individual `clientId` or `userId`.
#' 
#' Bear in mind each call will count against your API quota, so fetching a large amount of client ids will be limited by that.
#' 
#' @export
#' 
#' @return A list of data.frames: \code{$sessions} contains session level data. \code{$hits} contains individual activity data
#' @importFrom googleAuthR gar_api_generator gar_api_page
#' @import assertthat
#' @examples 
#' 
#' \dontrun{
#' 
#' # access data for individual users
#' uar <- ga_clientid_activity(c("1106980347.1461227730", "476443645.1541099566"),
#'                          viewId = 81416156, 
#'                          date_range = c("2019-01-01","2019-02-01"))
#' 
#' # access the data.frames returned:
#' 
#' # the session level data for the users passed in
#' uar$sessions
#' 
#' # the hit level activity for the users passed in
#' uar$hits
#'
#' # filter the response to only include certain activity types, such as goals:
#' 
#' only_goals <- ga_clientid_activity(c("1106980347.1461227730", 
#'                                      "476443645.1541099566"),
#'                      viewId = 81416156, 
#'                      date_range = c("2019-01-01","2019-02-01"),
#'                      activity_types = "GOAL") 
#'
#' # to access all activity use google_analytics() to fetch the clientId dimension
#' viewId <- 123456
#' date_range <- c("yesterday","yesterday")
#' cids <- google_analytics(viewId, date_range = date_range, 
#'                          metrics = "sessions", dimensions = "clientId")
#' 
#' users <- ga_clientid_activity(cids$clientId,
#'                               viewId = viewId, 
#'                               date_range = date_range)                     
#' 
#' 
#' }
#' @seealso \url{https://developers.google.com/analytics/devguides/reporting/core/v4/rest/v4/userActivity/search}
#' @importFrom purrr map map_dfr
ga_clientid_activity <- function(ids, 
                                 viewId, 
                                 id_type = c("CLIENT_ID","USER_ID"), 
                                 activity_type = NULL,
                                 date_range = NULL){
  ids     <- as.character(ids)
  viewId  <- as.character(viewId)
  id_type <- match.arg(id_type)
  
  results <- map(ids, 
                 ga_clientid_activity_one,
                 viewId = viewId,
                 id_type = id_type,
                 activity_type = activity_type,
                 date_range = date_range)
  
  structure(
    list(
      sessions = map_dfr(results, "session"),
      hits = map_dfr(results, "hits")
    ),
    class = c("ga_user_result", "list")
  )

}

#' @noRd
#' @importFrom purrr map_dfr
ga_clientid_activity_one <- function(id, 
                                     viewId, 
                                     id_type, 
                                     activity_type,
                                     date_range){
  
  myMessage("Fetching id: ", id, level = 3)
  
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
    
    possible_types <- c("PAGEVIEW","SCREENVIEW","GOAL","ECOMMERCE","EVENT")
    assert_that(
      is.character(activity_type)
    )
    
    if(!all(activity_type %in% possible_types)){
      stop("activity_type must be NULL or a vector of these types: ",
           paste(possible_types, collapse = " "), call. = FALSE)
    }
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
  

  out <- list(session = map_dfr(o, "session"),
              hits = map_dfr(o, "hits"))
  
  out$session$id <- id
  out$hits$id <- id
  
  # rate limit assuming 2000 in 100 seconds
  Sys.sleep(0.1)
  
  out
  
}

page_user_activity <- function(x){
  attr(x, "nextPageToken")
}



#' @noRd
#' @importFrom purrr map map_chr map_lgl
#' @importFrom dplyr bind_rows mutate left_join
#' @importFrom tibble enframe as_tibble
#' @importFrom tidyr unnest
parse_user_activity <- function(x){
  
  o_sess <- map_dfr(x$sessions, function(y){
    data.frame(sessionId = y$sessionId, 
               deviceCategory = y$deviceCategory, 
               platform = y$platform, 
               dataSource = y$dataSource,
               sessionDate = y$sessionDate,
               stringsAsFactors = FALSE)  
  })
  
  o_acts <- setNames(map(x$sessions, 
                function(sid){
                  setNames(sid$activities, 
                           map_chr(sid$activities, 
                                   ~format(timestamp_to_r(.x[["activityTime"]]))
                                   ))
                  }),
                map_chr(x$sessions, "sessionId"))
  
  activity <- NULL
  nested_hits <- o_acts %>% 
    tibble::enframe(name = "sessionId", value = "activity") %>% 
    tidyr::unnest(cols = activity) %>% 
    mutate(activityTime = iso8601_to_r(map_chr(activity, "activityTime")),
           source = map_chr(activity, "source"),
           medium = map_chr(activity, "medium"),
           channelGrouping = map_chr(activity, "channelGrouping"),
           campaign = map_chr(activity, "campaign"),
           keyword = map_chr(activity, "keyword"),
           hostname = map_chr(activity, "hostname"),
           landingPagePath = map_chr(activity, "landingPagePath"),
           activityType = map_chr(activity, "activityType"),
           customDimension = map(activity, "customDimension"),
           pagePath = map_chr(activity, ~safe_extract(.x$pageview$pagePath)),
           pageTitle = map_chr(activity, ~safe_extract(.x$pageview$pageTitle)),
           screenName = map_chr(activity, ~safe_extract(.x$appview$screenName)),
           mobileDeviceBranding = map_chr(activity, 
                                          ~safe_extract(.x$appview$mobileDeviceBranding)),       
           mobileDeviceModel = map_chr(activity, 
                                       ~safe_extract(.x$appview$mobileDeviceModel)),
           appName = map_chr(activity, ~safe_extract(.x$appview$appName)),   
           ecommerce = map(activity, "ecommerce"),
           goals = map(map(activity, "goals"), "goals"),
           has_goal = map_lgl(goals, ~!is.null(.)),
           eventCategory = map_chr(activity, ~safe_extract(.x$event$eventCategory)),
           eventAction = map_chr(activity, ~safe_extract(.x$event$eventAction)),
           eventLabel = map_chr(activity, ~safe_extract(.x$event$eventLabel)),
           eventValue = map_chr(activity, ~safe_extract(.x$event$eventValue)),
           eventCount = map_chr(activity, ~safe_extract(.x$event$eventCount))
           )
  
  out_hits <- nested_hits %>% select(-activity)

  o <- list(session = o_sess,
            hits = out_hits
            )
  attr(o, "nextPageToken") <- x$nextPageToken
  attr(o, "totalRows") <- x$totalRows
  attr(o, "sampleRate") <- x$sampleRate
  
  if(as.numeric(x$sampleRate) < 1){
    myMessage("Data is sampled at a sample rate of ", as.numeric(o$sampleRate)*100, "%", 
              level = 3)
  }
  
  o
  
}

#' Unnest user activity columns
#' 
#' A function to help expand data out of nested columns
#' 
#' @param hits The hits data.frame with the columns to expand
#' @param column Which column to expand - one of \code{"customDimension","ecommerce","goals"}
#' 
#' @export
#' @import assertthat
#' @importFrom purrr map map_chr map_lgl
#' @importFrom dplyr select filter bind_cols mutate distinct
#' @importFrom tidyr unnest pivot_wider
ga_clientid_activity_unnest <- function(hits, column = c("customDimension","ecommerce","goals")){
  
  column <- match.arg(column)
  assert_that(is.data.frame(hits))
  
  if(!any(names(hits) %in% column)){
    stop("Couldn't find column ", column, " in passed hits data.frame", call. = FALSE)
  }
  
  na_or_value <- function(x) if(!is.null(x$value)) x$value else NA_character_
  
  if(column == "customDimension"){
    unnested <- hits %>%
      select(id, sessionId, activityTime, customDimension) %>%
      unnest(cols = customDimension) %>%
      mutate(cd_index = map_chr(customDimension, "index"),
             cd_value = map_chr(customDimension, na_or_value))   %>%
      filter(!is.na(cd_value)) %>%
      select(-customDimension) %>%
      distinct() %>%
      pivot_wider(names_from = cd_index, values_from = cd_value, names_prefix = "customDim")
  } else if(column == "goals"){
    unnested <- hit_data %>% 
      filter(has_goal) %>% # filter to just hits with goals
      select(id, sessionId, activityTime, goals) %>% 
      unnest(cols = goals) %>% # unnest the goals list column
      mutate(goalIndex = map_chr(goals, "goalIndex"), 
             goalName = map_chr(goals, "goalName"), 
             goalCompletionLocation = map_chr(goals, "goalCompletionLocation")) %>%
      select(-goals)
  } else {
    stop("ecommerce not supported yet", call. = FALSE)
  }
  
  bind_cols(hits, unnested)
  
}

