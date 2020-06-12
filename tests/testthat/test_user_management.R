source("setup.R")

context("User Management")

test_that("Can perform user management", {
  skip_on_cran()
  skip_on_travis()
  test_email <- "googleanalyticsr-tests@g-analytics-r.iam.gserviceaccount.com"
  acc <- ga_users_list(accountId)
  web <- ga_users_list(accountId, webPropertyId = webPropId)
  view <- ga_users_list(accountId, webPropertyId = webPropId, viewId = ga_id)
  
  expect_s3_class(acc, "data.frame")
  expect_true(test_email %in% acc$userRef.email)
  expect_s3_class(web, "data.frame")
  expect_s3_class(view, "data.frame")
  
  test_email_adds <- c("mark@iihnordic.com", "iihmarkedmondson@gmail.com")
  done <- ga_users_add(test_email_adds,
               permissions = "EDIT", accountId = accountId)
  expect_true(done)
  Sys.sleep(10)
  acc2 <- ga_users_list(accountId)
  expect_true(all(test_email_adds %in% acc2$userRef.email))
  
  ga_users_delete(test_email_adds, accountId = accountId)
  Sys.sleep(10)
  acc3 <- ga_users_list(accountId)
  expect_false(all(test_email_adds %in% acc3$userRef.email))
  
})

