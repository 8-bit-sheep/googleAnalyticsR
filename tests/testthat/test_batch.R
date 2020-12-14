source("setup.R")
  
  test_that("v3 multi account batching without flag", {
    context("Batch API Mocking")
    skip_on_cran()
    skip_on_travis()
    multi <- google_analytics_3(c(ga_id, ga_id2),
                              start = "2015-07-31", end = "2015-10-01",
                              dimensions=c('medium'),
                              metrics = c('sessions'),
                              sort = "ga:sessions")

    expect_length(multi, 2)

    expect_s3_class(multi[[1]], "data.frame")
    expect_s3_class(multi[[2]], "data.frame")

  })

  test_that("v3 multi account batching with flag", {
    skip_on_cran()
    skip_on_travis()

    multi <- google_analytics_3(c(ga_id2, ga_id),
                              start = "2015-07-30", end = "2015-10-01",
                              dimensions=c('medium'),
                              metrics = c('sessions'),
                              sort = "ga:sessions",
                              multi_account_batching = TRUE)

    expect_length(multi, 2)

    expect_s3_class(multi[[1]], "data.frame")
    expect_s3_class(multi[[2]], "data.frame")

  })
  
  test_that("v3 WALK data", {
    skip_on_cran()
    skip_on_travis()

    walked <- suppressWarnings(
      google_analytics_3(ga_id,
                       start = "2017-07-30", end = "2017-08-01",
                       dimensions=c('medium'),
                       metrics = c('sessions'),
                       sort = "ga:sessions",
                       samplingLevel = "WALK")
      )

    expect_s3_class(walked, "data.frame")

  })

  test_that("v3 Batch data", {
    skip_on_cran()
    skip_on_travis()

    bb <-       google_analytics_3(ga_id,
                                 start = "2015-07-30", end = "2016-10-01",
                                 dimensions=c('medium','source','hour','pagePath'),
                                 metrics = c('sessions'),
                                 sort = "ga:sessions",
                                 max_results = 30000)

    expect_s3_class(bb, "data.frame")

  })

  test_that("v4 API matches v3 equivalent API call", {
    skip_on_cran()
    skip_on_travis()
    v3 <- google_analytics_3(ga_id,
                           start = "2015-07-30", end = "2015-10-01",
                           dimensions=c('medium'),
                           metrics = c('sessions'),
                           sort = "ga:sessions")
    v4 <-  google_analytics(ga_id,
                              date_range = c("2015-07-30","2015-10-01"),
                              dimensions=c('medium'),
                              metrics = c('sessions'),
                              order = order_type("sessions"))

    expect_equal(v3$medium, v4$medium)
    expect_equal(v3$sessions, v4$sessions)

  })
