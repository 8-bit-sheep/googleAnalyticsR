library(googleAnalyticsR)
library(testthat)

context("Authentication")

test_that("Auth check", {
  skip_on_cran()
  expect_s3_class(gar_auth("httr-oauth.rds"), "Token2.0")
})



context("Data fetching")

ga_id <- 106249469

test_that("Vanilla test data fetch", {
  skip_on_cran()
  t11 <-   google_analytics_4(ga_id, 
                              date_range = c("2015-07-30","2015-10-01"),
                              dimensions=c('medium'), 
                              metrics = c('sessions'),
                              order = order_type("sessions"))  
  expect_s3_class(t11, "data.frame")
})

test_that("v4 API matches v3 equivalent API call", {
  skip_on_cran()
  v3 <- google_analytics(ga_id, 
                         start = "2015-07-30", end = "2015-10-01",
                         dimensions=c('medium'), 
                         metrics = c('sessions'),
                         sort = "ga:sessions")  
  v4 <-  google_analytics_4(ga_id, 
                            date_range = c("2015-07-30","2015-10-01"),
                            dimensions=c('medium'), 
                            metrics = c('sessions'),
                            order = order_type("sessions")) 
  
  expect_equal(v3$medium, v4$medium)
  expect_equal(v3$sessions, v4$sessions)
  
})

test_that("Filters work", {
  skip_on_cran()
  ## create filters on metrics
  mf <- met_filter("bounces", "GREATER_THAN", 0)
  mf2 <- met_filter("sessions", "GREATER", 2)
  
  ## create filters on dimensions
  df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
  df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)
  
  ## construct filter objects
  fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
  fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")
  
  out <- google_analytics_4(ga_id, 
                     date_range = c("2015-07-30","2015-10-01"),
                     dimensions=c('source','medium'), 
                     metrics = c('sessions','bounces'), 
                     met_filters = fc, 
                     dim_filters = fc2, 
                     filtersExpression = "ga:source!=(direct)")
  
  expect_s3_class(out, "data.frame")
  expect_gt(min(out$bounces), 0)
  expect_gt(min(out$sessions), 2)
  
  source_beginnings <- unique(vapply(out$source, function(x) substr(x, 1, 1), character(1)))
  expect_false("1" %in% source_beginnings)
  expect_false("a" %in% source_beginnings)
  
})


context("Upload")

test_that("Can upload a data.frame ", {
 
  upload_me <- data.frame(medium = "shinyapps", 
                          source = "referral", 
                          adCost = 1, 
                          date = "20160801")
  
  rr <- ga_custom_upload_file(47480439, "UA-47480439-2", "_jDsJHSFSU-uw038Bh8fUg", upload_me)
  expect_equal(rr$kind, "analytics#upload")
  
  new_rr <- ga_custom_upload(upload_object = rr)
  expect_equal(rr$kind, "analytics#upload")
})