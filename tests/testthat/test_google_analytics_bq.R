context("BigQuery")

test_that("Can make SQL correctly", {
 
  expected <- "SELECT trafficSource.source as source, SUM(totals.visits) as sessions FROM (TABLE_DATE_RANGE([foo.ga_sessions_], TIMESTAMP('2017-01-01'), TIMESTAMP('2017-02-01'))) GROUP BY source  LIMIT 100"
  
  got <- google_analytics_bq("blah","foo", 
                             start = "2017-01-01", end = "2017-02-01", 
                             metrics = "sessions", dimensions = "source", 
                             return_query_only = TRUE)
  
  expect_equal(got, expected = expected)
  
})

test_that("Error if invalid dimension", {

  expect_error(google_analytics_bq("blah","foo", 
                                   start = "2017-01-01", end = "2017-02-01", 
                                   metrics = "sessions", dimensions = "adWordsQuery", 
                                   return_query_only = TRUE),
               regexp = "invalid dimension name")
})

test_that("Error if invalid metric", {
  
  expect_error(google_analytics_bq("blah","foo", 
                                   start = "2017-01-01", end = "2017-02-01", 
                                   metrics = "sales", dimensions = "source", 
                                   return_query_only = TRUE),
               regexp = "invalid metric name")
  
})

test_that("Null metric", {
  
  expected <- "SELECT trafficSource.source as source,  FROM (TABLE_DATE_RANGE([foo.ga_sessions_], TIMESTAMP('2017-01-01'), TIMESTAMP('2017-02-01'))) GROUP BY source  LIMIT 100"
  
  got <- google_analytics_bq("blah","foo", 
                             start = "2017-01-01", end = "2017-02-01", 
                             dimensions = "source", 
                             return_query_only = TRUE)
  expect_equal(got, expected = expected)
})

test_that("Null dimension", {
  
  expect_error(google_analytics_bq("blah","foo", 
                                   start = "2017-01-01", end = "2017-02-01", 
                                   metrics = "sessions", 
                                   return_query_only = TRUE),
               regexp = "At least one dimension")
})

