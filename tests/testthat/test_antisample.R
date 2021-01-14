test_that("Anti-sample when no sampling there", {
  skip_on_cran()
  skip_on_travis()
  as <-   google_analytics(ga_id,
                           date_range = c("2015-07-30","2018-10-01"),
                           dimensions=c('minute','hour','landingPagePath','medium','eventLabel','campaign'),
                           metrics = c('sessions'),
                           anti_sample = TRUE)
  expect_s3_class(as, "data.frame")
  
  
})

test_that("Aggregation works", {
  skip_on_cran()
  skip_on_travis()
  as <-   google_analytics(ga_id, 
                           date_range = c("2015-07-30","2016-10-01"),
                           dimensions=c('minute','hour','landingPagePath',
                                        'medium','eventLabel','campaign'), 
                           metrics = c('sessions', 'avgSessionDuration'),
                           anti_sample = TRUE)  
  expect_s3_class(as, "data.frame")
  
  agg <- aggregateGAData(as, agg_names = "medium")
  expect_equal(names(agg), c("medium", "sessions", "avgSessionDuration"))
  expect_lt(max(agg$avgSessionDuration), max(as$avgSessionDuration))
  
})
