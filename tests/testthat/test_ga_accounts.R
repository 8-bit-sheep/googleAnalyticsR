library(testthat)
library(googleAuthR)
gar_cache_setup("googleAnalyticsR", location = "mock")

library(googleAnalyticsR)

context("Accounts")

test_that("Get the account summary list", {
  skip_on_cran()
  al <- ga_account_list()
  expect_s3_class(al, "data.frame")
  
})
