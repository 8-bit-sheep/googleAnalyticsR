source("setup.R")

test_that("Big v4 batch", {
  skip_on_cran()
  skip_on_travis()
  big <-   google_analytics(ga_id, 
                            date_range = c("2015-07-30","2015-12-31"),
                            dimensions=c('medium','source','hour',
                                         'minute','campaign','pagePath'), 
                            metrics = c('sessions'),
                            max = -1)  
  expect_s3_class(big, "data.frame")
  
})

test_that("Slow big v4 batch", {
  skip_on_cran()
  skip_on_travis()
  big <-   google_analytics(ga_id, 
                            date_range = c("2015-07-30","2015-09-01"),
                            dimensions=c('medium','source','hour',
                                         'minute','campaign','pagePath'), 
                            metrics = c('sessions'),
                            max = -1,
                            slow_fetch = TRUE)  
  expect_s3_class(big, "data.frame")
})
