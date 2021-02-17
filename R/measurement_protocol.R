#' Make a Measurement Protocol v2 request
#' 
#' @description 
#' Create a server side call to Google Analytics 4 via its Measurement Protocol
#' 
#' @param events The events to send
#' @param client_id The client_id to associate with the event
#' @param measurement_id The measurement ID associated with a stream
#' @param api_secret The secret generated in the GA4 UI
#' @param debug Send hits to the debug endpoint to validate hits
#' @param user_id Optional. Unique id for the user
#' @param timestamp_micros Optional. A Unix timestamp (in microseconds) for the time to associate with the event. 
#' @param user_properties Optional. The user properties for the measurement.
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
#' `non_personalized_ads` - Optional. Set to TRUE to indicate these events should not be used for personalized ads. 
#' 
#'   
#' @export
ga_mp_send <- function(events,
                       client_id,
                       measurement_id,
                       api_secret,
                       debug = FALSE,
                       user_id = NULL,
                       timestamp_micros = NULL,
                       user_properties = TRUE,
                       non_personalised_ads = NULL){
  
  assert_that(
    is.string(client_id),
    is.string(measurement_id),
    is.string(api_secret),
    is.flag(debug)
  )
  
  endpoint <- "https://www.google-analytics.com/mp/collect"
  if(debug) endpoint <- "https://www.google-analytics.com/debug/mp/collect"
  
  the_url <- sprintf(
    "%s?measurement_id=%s&api_secet=%s",
    endpoint, measurement_id, api_secret
  )
  
  the_body <- rmNullObs(list(
    client_id = client_id,
    user_id = user_id,
    timestamp_micros = timestamp_micros,
    user_properties = user_properties,
    non_personalised_ads = non_personalised_ads,
    events = events
  ))
  
  httr::POST(
    the_url,
    body = the_body
  )
}

#' Create a Measurement Protocol Event
#' 
#' @description 
#' This creates an event to send via [ga_mp_send]
#' 
#' @param name The event name to send in
#' @param param Optional event parameters
#' 
#' @export
ga_mp_event <- function(name, params = NULL){
  
  structure(
    rmNullObs(list(
      name = name,
      params = params,
    )), class = "ga_mp_event"
  )
}

#' Create an Measurement Protocol Item Property for an Event
#' 
#' @description 
#' Some events work with item properties
#' 
#' @param item_id Item ID
#' @param item_name Item Name
#' @param coupon Coupon
#' @param discount Discount
#' @param affiliation Afilliation
#' @param item_brand Brand
#' @param item_category Category
#' @param item_variant Variant
#' @param price Price
#' @param currency Currency
#' 
#' @export
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
    )), class = "ga_mp_event_item"
  )
  
}
