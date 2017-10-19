library(httptest)
.mockPaths("..")

options(googleAuthR.scopes.selected = 
          c("https://www.googleapis.com/auth/analytics",
            "https://www.googleapis.com/auth/analytics.edit",
            "https://www.googleapis.com/auth/analytics.manage.users.readonly",
            "https://www.googleapis.com/auth/cloud-platform",
            "https://www.googleapis.com/auth/devstorage.full_control"),
        googleAuthR.client_id = "289759286325-da3fr5kq4nl4nkhmhs2uft776kdsggbo.apps.googleusercontent.com",
        googleAuthR.client_secret = "1mKySbffYRyWevGkjL0LMJYu")

library(googleAnalyticsR)



accountId <- 54019251
webPropId <- "UA-54019251-4"
ga_id <- 106249469

accountId2 <- 47480439
webPropId2 <- "UA-47480439-2"
ga_id2 <- 81416156

local_auth <- "GA_AUTH_FILE"

context("Batch API Mocking")

test_that("Record requests if online", {
  skip_if_disconnected()
  skip_if_no_env_auth(local_auth)
  
  ## test reqs
  capture_requests(
    {
      google_analytics(c(ga_id, ga_id2),
                                start = "2015-07-31", end = "2015-10-01",
                                dimensions=c('medium'),
                                metrics = c('sessions'),
                                sort = "ga:sessions")
      
      google_analytics(c(ga_id2, ga_id),
                       start = "2015-07-30", end = "2015-10-01",
                       dimensions=c('medium'),
                       metrics = c('sessions'),
                       sort = "ga:sessions",
                       multi_account_batching = TRUE)
      
      #   google_analytics(ga_id,
      #                    start = "2015-07-30", end = "2015-10-01",
      #                    dimensions=c('medium'), 
      #                    metrics = c('sessions'),
      #                    sort = "ga:sessions",
      #                    samplingLevel = "WALK")
      # 
      # google_analytics(ga_id,
      #                        start = "2015-07-30", end = "2015-10-01",
      #                        dimensions=c('medium','source','hour','minute','pagePath'), 
      #                        metrics = c('sessions'),
      #                        sort = "ga:sessions",
      #                        max_results = 30000)
      # 
      # google_analytics(ga_id, 
      #                        start = "2015-07-30", end = "2015-10-01",
      #                        dimensions=c('medium'), 
      #                        metrics = c('sessions'),
      #                        sort = "ga:sessions")  
      
      
    })
  
  
  
})


with_mock_API({
  
  test_that("v3 multi account batching without flag", {

    skip_on_cran()
    multi <- google_analytics(c(ga_id, ga_id2),
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

    multi <- google_analytics(c(ga_id2, ga_id),
                              start = "2015-07-30", end = "2015-10-01",
                              dimensions=c('medium'),
                              metrics = c('sessions'),
                              sort = "ga:sessions",
                              multi_account_batching = TRUE)

    expect_length(multi, 2)

    expect_s3_class(multi[[1]], "data.frame")
    expect_s3_class(multi[[2]], "data.frame")

  })

})