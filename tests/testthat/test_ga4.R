library(googleAnalyticsR)
library(testthat)

context("Authentication")

test_that("Auth check", {
  skip_on_cran()
  skip_on_travis()
  expect_s3_class(ga_auth(), "Token2.0")
})

ga_id <- 106249469

context("Data fetching")
vanilla_test <- function(){
  skip_on_cran()
  skip_on_travis()
  google_analytics_4(ga_id, 
                     date_range = c("2015-07-30","2015-10-01"),
                     dimensions=c('medium'), 
                     metrics = c('sessions'),
                     order = order_type("sessions"))  
}

vanilla_test2 <- function(){
  skip_on_cran()
  skip_on_travis()
  google_analytics(ga_id, 
                   start = "2015-07-30", end = "2015-10-01",
                   dimensions=c('medium'), 
                   metrics = c('sessions'),
                   sort = "ga:sessions")  
}

test_that("Vanilla test data fetch", {
  skip_on_cran()
  skip_on_travis()
  expect_equal_to_reference(vanilla_test(), "t1.rds")
})

test_that("v4 API matches v3 equivalent API call", {

  expect_equivalent(vanilla_test(), vanilla_test2())
})

example_filter <- function(){
  skip_on_cran()
  skip_on_travis()
  ## create filters on metrics
  mf <- met_filter("bounces", "GREATER_THAN", 0)
  mf2 <- met_filter("sessions", "GREATER", 2)
  
  ## create filters on dimensions
  df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
  df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)
  
  ## construct filter objects
  fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
  fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")
  google_analytics_4(ga_id, 
                     date_range = c("2015-07-30","2015-10-01"),
                     dimensions=c('source','medium'), 
                     metrics = c('sessions','bounces'), 
                     met_filters = fc, 
                     dim_filters = fc2, 
                     filtersExpression = "ga:source!=(direct)")
}

example_multidate <- function(){
  skip_on_cran()
  skip_on_travis()
  ## demo of querying two date ranges at a time   
  ## we make the request via make_ga_4_req() to use in next demo
  multidate_test <- make_ga_4_req(ga_id, 
                                  date_range = c("2015-07-30",
                                                 "2015-10-01",
                                                 "2014-07-30",
                                                 "2014-10-01"),
                                  dimensions = c('source','medium'), 
                                  metrics = c('sessions','bounces'))
  
  fetch_google_analytics_4(list(multidate_test))
}

example_multifetch <- function(){
  skip_on_cran()
  skip_on_travis()
  multidate_test <- make_ga_4_req(ga_id, 
                                  date_range = c("2015-07-30",
                                                 "2015-10-01",
                                                 "2014-07-30",
                                                 "2014-10-01"),
                                  dimensions = c('source','medium'), 
                                  metrics = c('sessions','bounces'))
  ## Demo querying two reports at the same time
  ## Use make_ga_4_req() to make multiple requests and then send 
  ##   them as a list to fetch_google_analytics_4()
  multi_test2 <- make_ga_4_req(ga_id,
                               date_range = c("2015-07-30",
                                              "2015-10-01",
                                              "2014-07-30",
                                              "2014-10-01"),
                               dimensions=c('hour','medium'), 
                               metrics = c('visitors','bounces'))
  
  ## all requests must have same viewID and dateRange
  fetch_google_analytics_4(list(multidate_test, multi_test2)) 
}

example_calculated <- function(){
  skip_on_cran()
  skip_on_travis()
  google_analytics_4(ga_id,
                     date_range = c("2015-07-30",
                                    "2015-10-01"),
                     dimensions=c('medium'), 
                     metrics = c(visitsPerVisitor = "ga:visits/ga:visitors",
                                 'bounces'), 
                     metricFormat = c("FLOAT","INTEGER"))
}

example_simple_segment <- function(){
  skip_on_cran()
  skip_on_travis()
  ## make a segment element
  se <- segment_element("sessions", 
                        operator = "GREATER_THAN", 
                        type = "metric", 
                        comparisonValue = 1, 
                        scope = "USER")
  
  se2 <- segment_element("medium", 
                         operator = "EXACT", 
                         type = "dimension", 
                         expressions = "organic")
  
  ## choose between segment_vector_simple or segment_vector_sequence
  ## Elements can be combined into clauses, which can then be combined into OR filter clauses
  sv_simple <- segment_vector_simple(list(list(se)))
  
  sv_simple2 <- segment_vector_simple(list(list(se2)))
  
  ## Each segment vector can then be combined into a logical AND
  seg_defined <- segment_define(list(sv_simple, sv_simple2))
  
  ## Each segement defintion can apply to users, sessions or both.
  ## You can pass a list of several segments
  segment4 <- segment_ga4("simple", user_segment = seg_defined)
  
  ## Add the segments to the segments param
  google_analytics_4(ga_id, 
                     c("2015-07-30","2015-10-01"), 
                     dimensions=c('source','medium','segment'), 
                     segments = segment4, 
                     metrics = c('sessions','bounces')
  )
}

example_sequence_segment <- function(){
  skip_on_cran()
  skip_on_travis()
  se2 <- segment_element("medium", 
                         operator = "EXACT", 
                         type = "dimension", 
                         expressions = "organic",
                         matchType = "PRECEDES")
  
  se3 <- segment_element("medium",
                         operator = "EXACT",
                         type = "dimension",
                         not = TRUE,
                         expressions = "organic",
                         matchType = "PRECEDES")
  
  ## step sequence
  ## users who arrived via organic then via referral
  sv_sequence <- segment_vector_sequence(list(list(se2), 
                                              list(se3)))
  
  seq_defined2 <- segment_define(list(sv_sequence))
  
  segment4_seq <- segment_ga4("sequence", user_segment = seq_defined2)
  
  ## Add the segments to the segments param
  google_analytics_4(ga_id, 
                     c("2016-04-01","2016-05-01"), 
                     dimensions=c('source','segment'), 
                     segments = segment4_seq,
                     metrics = c('sessions','bounces')
  )
}

example_cohorts <- function(){
  skip_on_cran()
  skip_on_travis()
  ## first make a cohort group
  cohort4 <- make_cohort_group(list("Jan2016" = c("2016-01-01", "2016-01-31"), 
                                    "Feb2016" = c("2016-02-01","2016-02-28")))
  
  ## then call cohort report.  No date_range and must include metrics and dimensions
  ##   from the cohort list
  google_analytics_4(ga_id, 
                     dimensions=c('cohort'), 
                     cohorts = cohort4, 
                     metrics = c('cohortTotalUsers'))
}

example_pivots <- function(){
  skip_on_cran()
  skip_on_travis()
  mf2 <- met_filter("sessions", "GREATER", 100)
  
  fc <- filter_clause_ga4(list(mf2))
  ## filter pivot results to 
  pivot_dim_filter1 <- dim_filter("medium",
                                  "REGEXP",
                                  "organic|social|email|cpc")
  
  pivot_dim_clause <- filter_clause_ga4(list(pivot_dim_filter1))
  
  pivme <- pivot_ga4("medium",
                     metrics = c("sessions"), 
                     maxGroupCount = 3, 
                     dim_filter_clause = pivot_dim_clause)
  
  google_analytics_4(ga_id, 
                     c("2016-01-30","2016-10-01"), 
                     dimensions=c('source'), 
                     metrics = c('sessions'), 
                     pivots = list(pivme),
                     met_filters = fc,
                     order = order_type("sessions"))
  
}

test_that("Examples all run", {
  expect_equal_to_reference(example_filter(), file = "t2.rds")
  expect_equal_to_reference(example_multidate(), file = "t3.rds")  
  # expect_equal_to_reference(example_multifetch(), file = "t4.rds")
  expect_equal_to_reference(example_calculated(), file = "t5.rds")
  # expect_equal_to_reference(example_simple_segment(), file = "t6.rds")
  # expect_equal_to_reference(example_sequence_segment(), file = "t7.rds")
  expect_equal_to_reference(example_cohorts(), file = "t8.rds")  
  # expect_equal_to_reference(example_pivots(), file = "t9.rds")  
})

context("Upload")

test_that("Can upload a data.frame ", {
 
  upload_me <- data.frame(medium = "shinyapps", 
                          source = "referral", 
                          adCost = 1, 
                          date = "20160801")
  
  rr <- ga_custom_upload_file(47480439, "UA-47480439-2", "_jDsJHSFSU-uw038Bh8fUg", upload_me)
  expect_equal(rr$kind, "analytics#upload")
})