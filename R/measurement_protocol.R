#' Make a Measurement Protocol v2 request
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' Create a server side call to Google Analytics 4 via its Measurement Protocol
#' 
#' @param events The events to send
#' @param client_id The client_id to associate with the event
#' @param measurement_id The measurement ID associated with a stream
#' @param api_secret The secret generated in the GA4 UI - by default will look for environment arg `GA_MP_SECRET`
#' @param debug Send hits to the debug endpoint to validate hits
#' @param user_id Optional. Unique id for the user
#' @param timestamp_micros Optional. A Unix timestamp (in microseconds) for the time to associate with the event. 
#' @param user_properties Optional. The user properties for the measurement sent in as a named list.
#' @param non_personalized_ads Optional. Set to true to indicate these events should not be used for personalized ads. 
#' 
#' @details 
#' 
#' Create an API secret via `Admin > Data Streams > choose your stream > Measurement Protocol > Create`
#' 
#' To see event parameters, create custom fields in your GA4 account first, to see them in your reports 24hrs after you send them in with this function via `Custom definitions > Create custom dimensions` - `dimension name` will be how it looks like in the reports, `event parameter` will be the parameter you have sent in with the event.
#' 
#' `user_id` can be used for [cross-platform analysis](https://support.google.com/analytics/answer/9213390)
#' 
#' `timestamp_micros` should only be set to record events that happened in the past. This value can be overridden via user_property or event timestamps. Events can be backdated up to 48 hours. Note microseconds, not milliseconds.
#' 
#' `user_properties` - describe segments of your user base, such as language preference or geographic location.  See [User properties](https://developers.google.com/analytics/devguides/collection/protocol/ga4/user-properties?client_type=gtag) 
#' 
#' Ensure you also have user permission as specified in the [feature policy](https://developers.google.com/analytics/devguides/collection/protocol/ga4/policy)
#' 
#' Invalid events are silently rejected with a 204 response, so use `debug=TRUE` to validate your events first.
#' 
#' @seealso [Measurement Protocol (Google Analytics 4)](https://developers.google.com/analytics/devguides/collection/protocol/ga4)
#'   
#' @export
#' @family Measurement Protocol functions
#' @return `TRUE` if successful, if `debug=TRUE` then validation messages if not a valid hit.
#' @examples 
#' # preferably set this in .Renviron
#' Sys.setenv(GA_MP_SECRET="MY_SECRET")
#' 
#' # your GA4 settings
#' my_measurement_id <- "G-43MDXK6CLZ"
#' a_client_id <- 1234567
#' 
#' event <- ga_mp_event("an_event")
#' ga_mp_send(event, a_client_id, my_measurement_id, debug = TRUE)
#' 
#' another <- ga_mp_event("another_event")
#' ga_mp_send(list(event, another), 
#'            a_client_id, my_measurement_id, 
#'            debug = TRUE)
#' \dontrun{
#' # you can see sent events in the real-time reports
#' my_property_id <- 206670707
#' ga_data(my_property_id, 
#'         dimensions = "eventName", 
#'         metrics = "eventCount", 
#'         dim_filters = ga_data_filter(
#'            eventName == c("an_event","another_event")),
#'         realtime = TRUE)
#' 
#' }
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr content POST
ga_mp_send <- function(events,
                       client_id,
                       measurement_id,
                       user_id = NULL,
                       api_secret = Sys.getenv("GA_MP_SECRET"),
                       debug = FALSE,
                       timestamp_micros = NULL,
                       user_properties = NULL,
                       non_personalized_ads = TRUE){
  
  assert_that(
    is.string(measurement_id),
    is.string(api_secret),
    is.flag(debug),
    is.flag(non_personalized_ads)
  )
  
  if(length(events) > 0 && 
     !is.ga_mp_event(events) && 
     !all(unlist(lapply(events, is.ga_mp_event)))){
    stop("Must supply a ga_mp_event object or a list of ga_mp_event objects", 
         call. = FALSE)
  }
  
  endpoint <- "https://www.google-analytics.com/mp/collect"
  if(debug) endpoint <- "https://www.google-analytics.com/debug/mp/collect"
  
  the_url <- sprintf(
    "%s?measurement_id=%s&api_secret=%s",
    endpoint, measurement_id, api_secret
  )
  
  the_body <- rmNullObs(list(
    client_id = as.character(client_id),
    user_id = as.character(user_id),
    timestamp_micros = timestamp_micros,
    user_properties = user_properties,
    non_personalized_ads = non_personalized_ads,
    events = events
  ))
  
  if(debug || getOption("googleAuthR.verbose") < 3){
    myMessage("MP Request:", the_url,"\n", 
              toJSON(the_body, auto_unbox = TRUE, pretty = TRUE), 
              level = 3)
  }
  
  res <- POST(
    the_url,
    body = the_body,
    encode = "json"
  )
  
  myMessage("Response: ", res$status, level = 3)

  parsed <- content(res, as = "text", encoding = "UTF-8")
  
  if(nzchar(parsed) && debug){
    o <- fromJSON(parsed)
    if(length(o$validationMessages) > 0) return(o$validationMessages)
    myMessage("No validation messages found", level = 3)
  }
  
  TRUE
}

#' Create a Measurement Protocol Event
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This creates an event to send via [ga_mp_send]
#' 
#' @param name The event name to send in
#' @param params Optional event parameters sent in as a named list
#' @param items Optional items created via [ga_mp_event_item]
#' 
#' @export
#' @family Measurement Protocol functions
#' @examples 
#' 
#' ga_mp_event("custom_event")
#' ga_mp_event("custom_event", params = list(my_param = "SUPER"))
ga_mp_event <- function(name, params = NULL, items = NULL){
  
  if(!is.null(items)){
    params <- c(params, list(items = items))
  }
  
  structure(
    rmNullObs(list(
      name = name,
      params = params
    )), class = c("ga_mp_event","list")
  )
}

is.ga_mp_event <- function(x){
  inherits(x, "ga_mp_event")
}

#' @export
print.ga_mp_event <- function(x, ...){
  cat("\n==GA4 MP Event\n")
  print(jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE))
}

#' Create an Measurement Protocol Item Property for an Event
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' Some events work with item properties
#' 
#' @param item_id Item ID
#' @param item_name Item Name
#' @param coupon Coupon
#' @param discount Discount
#' @param affiliation Affiliation
#' @param item_brand Brand
#' @param item_category Category
#' @param item_variant Variant
#' @param price Price
#' @param currency Currency
#' 
#' @export
#' @family Measurement Protocol functions
#' @examples 
#' # one item
#' ga_mp_event_item(item_name = "jeggings", 
#'                  price = 8.88, 
#'                  item_variant = "Black")
#'                  
#' # many items in a list
#' items <- list(
#'   ga_mp_event_item(item_id = "SKU_12345", 
#'                    price = 9.99, 
#'                    item_brand = "Gucci"), 
#'   ga_mp_event_item(item_name = "jeggings", 
#'                    price = 8.88, 
#'                    item_variant = "Black"))
#'                    
#' # construct an event with its own fields
#' ga_mp_event("add_payment_info", 
#'             params = list(coupon = "SUMMER_FUN", 
#'                           payment_type = "Credit Card", 
#'                           value = 7.77, 
#'                           currency = "USD"), 
#'             items = items)
#'
ga_mp_event_item <- function(
  item_id = NULL,
  item_name = NULL,
  coupon = NULL,
  discount = NULL,
  affiliation = NULL,
  item_brand = NULL,
  item_category = NULL,
  item_variant = NULL,
  price = NULL,
  currency = NULL
){
  
  if(all(is.null(item_id), is.null(item_name))){
    stop("One of item_id or item_name is required")
  }
  
  structure(
    rmNullObs(list(
      item_id = item_id,
      item_name = item_name,
      coupon = coupon,
      discount = discount,
      affiliation = affiliation,
      item_brand = item_brand,
      item_category = item_category,
      item_variant = item_variant,
      price = price,
      currency = currency
    )), class = c("ga_mp_event_item","list")
  )
  
}

is.ga_mp_event_item <- function(x){
  inherits(x, "ga_mp_event_item")
}

#' @export
print.ga_mp_event_item <- function(x, ...){
  cat("==GA4 MP Event Item\n")
  print(jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE))
}

#' Generate a random client_id
#' 
#' This has a random number plus a timestamp
#' 
#' @param seed If you set a seed, then the random number will be the same for each value
#' 
#' @export
#' @family Measurement Protocol functions
ga_mp_cid <- function(seed = NULL){
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  rand <- round(stats::runif(1, min = 1, max = 100000000))
  ts <- round(as.numeric(Sys.time()))
  
  paste0(rand,".",ts)

}

#' Opt in or out of googleAnalyticsR usage tracking
#' 
#' You can opt-in or out to sending a measurement protocol hit when you load the package for use in the package's statistics via this function.  No personal data is collected.  
#' 
#' @export
#' @rdname ga_trackme_event
#' 
#' @examples 
#' 
#' # control your tracking choices via a menu if in interactive session
#' if(interactive()){
#'   ga_trackme()
#' }
#' @importFrom usethis ui_yeah
ga_trackme <- function(){
  
  cli::cli_h1("Tracking Consent for googleAnalyticsR usage")
  cli::cli_alert_info("This function opts you in or out of sending a tracking hit each time the library loads.  It is done using ga_trackme_event().")
  opt_in <- usethis::ui_yeah(
    "Do you opt in to tracking each time you load googleAnalyticsR?"
  )
  
  # this folder should exist as its used for gargle
  the_file <- .trackme$filepath
  
  if(opt_in){
    if(file.exists(the_file)){
      cli::cli_alert_info("Opt-in file {the_file} already found - no more action needed")
      return(invisible(NULL))
    }
    
    cli::cli_alert_success(
      "Thanks! Your consent and an ID will be marked by the presence of the file {the_file} in this folder.  Delete it or run this function again to remove consent.")
    
    file.create(the_file)
    cid <- ga_mp_cid()
    write(cid, the_file)

    return(invisible(NULL))
  }
  
  if(file.exists(the_file)){
    cli::cli_alert_info("Found {the_file} - deleting as you have opted-out")
    unlink(the_file)
  }
  
  cli::cli_alert_info(
    "No worries! If you change your mind run this function again.")
}  

.trackme <- new.env()
.trackme$measurement_id <- "G-43MDXK6CLZ"
.trackme$api <- "_hS_7VJARhqbCq9mF3oiNg"
.trackme$filepath <- file.path(path.expand('~'),".R","optin-googleanalyticsr")

#' Send a tracking hit for googleAnalyticsR package statistics
#' 
#' If you opt in, `ga_trackme_event()` is the function that fires.  You can use `debug=TRUE` to see what would be sent before opting in or out.
#' 
#' Running `ga_trackme_event()` function will send a Measurement Protocol hit via [ga_mp_send] only if the `~/.R/optin-googleanalyticsr` file is present
#' 
#' @param debug Set as a debug event to see what would be sent
#' @param say_hello If you want to add your own custom message to the event sent, add it here!
#' 
#' @export
#' 
#' @examples 
#' 
#' # this only works with a valid opt-in file present 
#' ga_trackme_event()
#' 
#' # see what data is sent
#' ga_trackme_event(debug=TRUE)
#' 
#' # add your own message!
#' ga_trackme_event(debug = TRUE, say_hello = "err hello Mark")
ga_trackme_event <- function(debug = FALSE, say_hello = NULL){
  
  assert_that_ifnn(say_hello, is.string)
  
  the_file <- .trackme$filepath
  if(!file.exists(the_file) & !debug){
    myMessage("No consent file found", level = 2)
    return(FALSE)
  }
  
  ss <- utils::sessionInfo()
  event <- ga_mp_event(
    "googleanalyticsr_loaded",
    params = list(
      r_version = ss$R.version$version.string,
      r_platform = ss$platform,
      r_locale = ss$locale,
      r_system = ss$running,
      say_hello = say_hello,
      package = paste("googleAnalyticsR",  utils::packageVersion("googleAnalyticsR"))
    )
  )
  
  if(debug){
    cid <- tryCatch(cid <- readLines(the_file)[[1]], 
                    error = function(e) "12345678.987654")
  } else {
    cid <- readLines(the_file)[[1]]
  }
  
  
  if(is.null(.trackme$measurement_id) | is.null(.trackme$api)){
    myMessage("No tracking parameters found, setting dummy values", level = 3)
    m_id = "Measurement_ID"
    api = "API_secret"
  } else {
    m_id = .trackme$measurement_id
    api = .trackme$api
  }
  
  
  if(debug){
    return(ga_mp_send(event, client_id = cid,
                      measurement_id = m_id,
                      api_secret = api,
                      debug = TRUE))
  }
  suppressMessages(
    ga_mp_send(event, client_id = cid,
               measurement_id = m_id,
               api_secret = api,
               debug = debug)
    )
  
  cli::cli_alert_success("Sent library load tracking event")
  
}
