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
#' @param user_properties Optional. The user properties for the measurement.
#' @param non_personalized_ads Optional. Set to true to indicate these events should not be used for personalized ads. 
#' 
#' @details 
#' 
#' Create an API secret via `Admin > Data Streams > choose your stream > Measurement Protocol > Create`
#' 
#' `user_id` can be used for [cross-platform analysis](https://support.google.com/analytics/answer/9213390)
#' 
#' `timestamp_micros` should only be set to record events that happened in the past. This value can be overridden via user_property or event timestamps. Events can be backdated up to 48 hours. Note microseconds, not milliseconds.
#' 
#' `user_properties` - describe segments of your user base, such as language preference or geographic location.  See [User properties](https://developers.google.com/analytics/devguides/collection/protocol/ga4/user-properties?client_type=gtag) 
#' 
#' @section ClientID:
#' 
#' In order for an event to be valid, it must have a client_id that has already been used to send an event from gtag.js. You will need to capture this ID client-side and include it in your call to the measurement protocol.  Ensure you also have user permission as specified in the [feature policy](https://developers.google.com/analytics/devguides/collection/protocol/ga4/policy)
#' 
#' @seealso [Measurement Protocol (Google Analytics 4)](https://developers.google.com/analytics/devguides/collection/protocol/ga4)
#'   
#' @export
#' @family Measurement Protocol functions
#' @examples 
#' # preferably set this in .Renviron
#' Sys.setenv(GA_MP_SECRET="MY_SECRET")
#' 
#' # your GA4 settings
#' my_measurement_id <- "G-43MDXK6CLZ"
#' a_client_id <- 1234567
#' 
#' event <- ga_mp_event("an_event")
#' ga_mp_send(event, my_measurement_id, 1234567, debug = TRUE)
#' 
#' another <- ga_mp_event("another_event")
#' ga_mp_send(list(event, another), my_measurement_id, 1234567, debug = TRUE)
#' 
ga_mp_send <- function(events,
                       measurement_id,
                       client_id,
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
              jsonlite::toJSON(the_body, auto_unbox = TRUE), 
              level = 3)
  }
  
  res <- httr::POST(
    the_url,
    body = the_body,
    encode = "json"
  )
  
  myMessage("Response: ", res$status, level = 3)
  
  o <- jsonlite::fromJSON(httr::content(res, as = "text"))
  
  if(debug){
    if(length(o$validationMessages) > 0) return(o$validationMessages)
    myMessage("No validation messages found", level = 3)
    return(invisible(NULL))
  }
  
  o
}

#' Create a Measurement Protocol Event
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' This creates an event to send via [ga_mp_send]
#' 
#' @param name The event name to send in
#' @param param Optional event parameters sent in as a named list
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
