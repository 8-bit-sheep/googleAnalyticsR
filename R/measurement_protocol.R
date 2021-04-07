#' Make a Measurement Protocol v2 request
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' Create a server side call to Google Analytics 4 via its Measurement Protocol
#' 
#' @param events The events to send
#' @param client_id The client_id to associate with the event
#' @param connection The connection details created by [ga_mp_connection]
#' @param debug_call Send hits to the Google debug endpoint to validate hits.
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
#' Invalid events are silently rejected with a 204 response, so use `debug_call=TRUE` to validate your events first.
#' 
#' @seealso [Measurement Protocol (Google Analytics 4)](https://developers.google.com/analytics/devguides/collection/protocol/ga4)
#'   
#' @export
#' @family Measurement Protocol functions
#' @return `TRUE` if successful, if `debug_call=TRUE` then validation messages if not a valid hit.
#' @examples 
#' # preferably set this in .Renviron
#' Sys.setenv(GA_MP_SECRET="MY_SECRET")
#' 
#' # your GA4 settings
#' my_measurement_id <- "G-1234"
#' 
#' my_connection <- ga_mp_connection(my_measurement_id)
#' 
#' a_client_id <- 123.456
#' event <- ga_mp_event("an_event")
#' ga_mp_send(event, a_client_id, my_connection, debug_call = TRUE)
#' 
#' # multiple events at same time in a batch
#' another <- ga_mp_event("another_event")
#' 
#' ga_mp_send(list(event, another), 
#'            a_client_id, 
#'            my_connection,
#'            debug_call = TRUE)
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
#' @importFrom measurementProtocol mp_send
ga_mp_send <- function(
  events,
  client_id,
  connection,
  user_id = NULL,
  debug_call = FALSE,
  timestamp_micros = NULL,
  user_properties = NULL,
  non_personalized_ads = TRUE){
  
  mp_send(events = events,
          client_id = client_id,
          connection = connection,
          user_id = user_id,
          debug_call = debug_call,
          timestamp_micros = timestamp_micros,
          user_properties = user_properties,
          non_personalized_ads = non_personalized_ads)
}

#' Create a connection for Measurement Protocol v2
#' 
#' Use [ga_mp_connection] to set up the Measurement Protocol connections to pass to [ga_mp_send].  If using Google Tag Manager Server-Side, you can also set up a custom endpoint.
#' 
#' @param api_secret The secret generated in the GA4 UI - by default will look for environment arg `GA_MP_SECRET`
#' @param measurement_id The measurement ID associated with a stream
#' @param endpoint If NULL will use Google default, otherwise set to the URL of your Measurement Protocol custom endpoint
#' @param preview_header Only needed for custom endpoints. The `X-Gtm-Server-Preview` HTTP Header found in your GTM debugger
#' @export
#' @examples 
#' 
#' # custom GTM server side endpoint
#' my_custom_connection <- ga_mp_connection(
#'    my_measurement_id,
#'    endpoint = "https://gtm.example.com",
#'    preview_header = "ZW52LTV8OWdPOExNWFkYjA0Njk4NmQ="
#'  )
#'  
#' @rdname ga_mp_send
#' @importFrom measurementProtocol mp_connection
ga_mp_connection <- function(measurement_id,
                             api_secret = Sys.getenv("GA_MP_SECRET"),
                             endpoint = NULL, 
                             preview_header = NULL){
  
  mp_connection(measurement_id = measurement_id,
                api_secret = api_secret,
                endpoint = endpoint,
                preview_header = preview_header)
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
#' @importFrom measurementProtocol mp_event
ga_mp_event <- function(name, params = NULL, items = NULL){
  
  mp_event(name = name, params = params, items = items)
  
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
#' @importFrom measurementProtocol mp_event_item
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
  
  mp_event_item(item_id = item_id,
                item_name = item_name,
                coupon = coupon,
                discount = discount,
                affiliation = affiliation,
                item_brand = item_brand,
                item_category = item_category,
                item_variant = item_variant,
                price = price,
                currency = currency)
  
}

#' Generate a random client_id
#' 
#' This has a random number plus a timestamp
#' 
#' @param seed If you set a seed, then the random number will be the same for each value
#' 
#' @export
#' @family Measurement Protocol functions
#' @importFrom measurementProtocol mp_cid
ga_mp_cid <- function(seed = NULL){
  
  mp_cid(seed = seed)

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
      "Thanks! Your consent and an ID will be marked by the presence of the file {the_file}.  Delete it or run this function again to remove consent.")
    
    dir.create(rappdirs::user_config_dir("googleAnalyticsR"), showWarnings = FALSE)
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
.trackme$filepath <- file.path(rappdirs::user_config_dir("googleAnalyticsR"), 
                               "optin-googleanalyticsr")

#' Send a tracking hit for googleAnalyticsR package statistics
#' 
#' If you opt in, `ga_trackme_event()` is the function that fires.  You can use `debug_call=TRUE` to see what would be sent before opting in or out.
#' 
#' Running `ga_trackme_event()` function will send a Measurement Protocol hit via [ga_mp_send] only if the `~/.R/optin-googleanalyticsr` file is present
#' 
#' @param debug_call Set as a debug event to see what would be sent
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
#' ga_trackme_event(debug_call=TRUE)
#' 
#' # add your own message!
#' ga_trackme_event(debug_call = TRUE, say_hello = "err hello Mark")
ga_trackme_event <- function(debug_call = FALSE, say_hello = NULL){
  
  assert_that_ifnn(say_hello, is.string)
  
  the_file <- .trackme$filepath
  if(!file.exists(the_file) & !debug_call){
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
  
  if(debug_call){
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
  
  my_conn <- ga_mp_connection(measurement_id = m_id, api_secret = api)
  
  if(debug_call){
    return(ga_mp_send(event, client_id = cid,
                      connection = my_conn,
                      debug_call = TRUE))
  }
  suppressMessages(
    ga_mp_send(event, client_id = cid,
               connection = my_conn,
               debug_call = debug_call)
    )
  
  cli::cli_alert_success("Sent library load tracking event")
  
}
