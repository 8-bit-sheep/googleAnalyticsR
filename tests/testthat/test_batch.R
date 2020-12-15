source("setup.R")

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
