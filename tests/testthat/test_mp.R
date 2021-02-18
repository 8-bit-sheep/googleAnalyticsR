test_that("Measurement Protocol Hits",{
  
  # preferably set this in .Renviron
  Sys.setenv(GA_MP_SECRET="MY_SECRET")
  
  # your GA4 settings
  my_measurement_id <- "G-43MDXK6CLZ"
  a_client_id <- 1234567
  
  event <- ga_mp_event("an_event")
  mp1 <- ga_mp_send(event, a_client_id, my_measurement_id, debug = TRUE)
  expect_true(mp1)
  
  event_error <- ga_mp_event("_an_event")
  mp3 <- ga_mp_send(event_error, a_client_id, my_measurement_id, debug = TRUE)
  expect_snapshot(mp3)
  
  another <- ga_mp_event("another_event")
  mp2 <- ga_mp_send(list(event, another), 
             a_client_id, my_measurement_id, 
             debug = TRUE)
  expect_true(mp2)
  
  # one item
  it1 <- ga_mp_event_item(item_name = "jeggings", 
                   price = 8.88, 
                   item_variant = "Black")
  expect_snapshot(it1)
  
  # many items in a list
  items <- list(
    ga_mp_event_item(item_id = "SKU_12345", 
                     price = 9.99, 
                     item_brand = "Gucci"), 
    it1)
  expect_snapshot(items)
  
  # construct an event with its own fields
  event1 <- ga_mp_event("add_payment_info", 
              params = list(coupon = "SUMMER_FUN", 
                            payment_type = "Credit Card", 
                            value = 7.77, 
                            currency = "USD"), 
              items = items)
  expect_snapshot(event1)

  
})