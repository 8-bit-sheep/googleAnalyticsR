source("setup.R")

context("Meta API Mocking")

test_that("Record requests if online", {
  skip_if_disconnected()
  googleAuthR::skip_if_no_env_auth(local_auth)
  
  ## test reqs
  capture_requests(
    {
      google_analytics_meta()
      ga_accounts()
      ga_account_list()
      ga_webproperty_list(accountId)
      ga_webproperty(accountId, "UA-54019251-1")
      ga_view_list(accountId, webPropId)
      ga_view(accountId, webPropertyId = webPropId, profileId = ga_id)      
      ga_adwords_list(accountId, webPropertyId = webPropId)      
      ga_adwords(accountId, 
                 webPropertyId = webPropId, 
                 webPropertyAdWordsLinkId = "34H8JW1_R4K3Nh4uZpsIvw")      
      ga_custom_datasource(accountId2, webPropId2)      
      ga_custom_upload_list(accountId2, 
                            webPropertyId = webPropId2, 
                            customDataSourceId = "_jDsJHSFSU-uw038Bh8fUg")      
      google_analytics_meta()
      gacl <- ga_custom_vars_list(accountId, webPropId, type = "customMetrics")
      gacl2 <- ga_custom_vars_list(accountId, webPropId, type = "customDimensions") 
      
      gacm <- ga_custom_vars(accountId, webPropId, type = "customDimensions", customId = "ga:dimension1") 
      
      goals <- ga_goal_list(accountId, webPropId, ga_id)
      
      goal <- ga_goal(accountId, webPropId, ga_id, 1)
      
      
      exper <- ga_experiment_list(accountId, webPropId, ga_id)
      
      
      
      acc <- ga_users_list(accountId)
      web <- ga_users_list(accountId, webPropertyId = webPropId)
      view <- ga_users_list(accountId, webPropertyId = webPropId, viewId = ga_id)
      
      
    })
  
  
  
})

context("Meta data")

test_that("Download meta data", {
  skip_on_cran()
  meta <- google_analytics_meta()
  
  expect_s3_class(meta, "data.frame")
  
})


context("Accounts")

test_that("Get the account list", {
  skip_on_cran()
  al <- ga_accounts()
  
  expect_equal(al$kind, "analytics#accounts")
  
})



test_that("Get the account summary list", {
  skip_on_cran()
  al <- ga_account_list()
  expect_s3_class(al, "data.frame")
  
})


context("Webproperties")

test_that("Get the webproperty list", {
  skip_on_cran()
  wb <- ga_webproperty_list(accountId)
  
  expect_equal(wb$kind, "analytics#webproperties")
  
})

test_that("Get the web property", {
  skip_on_cran()
  wb <- ga_webproperty(accountId, "UA-54019251-1")
  
  expect_equal(wb$kind, "analytics#webproperty")
  
})

context("Views")

test_that("Get the view list", {
  skip_on_cran()
  wb <- ga_view_list(accountId, webPropId)
  
  expect_equal(wb$kind, "analytics#profiles")
  
})

test_that("Get the individual View", {
  skip_on_cran()
  wb <- ga_view(accountId, webPropertyId = webPropId, profileId = ga_id)
  
  expect_equal(wb$kind, "analytics#profile")
  
})

context("AdWords")

test_that("Get Adwords list", {
  skip_on_cran()
  al <- ga_adwords_list(accountId, webPropertyId = webPropId)
  
  expect_equal(al$kind, "analytics#entityAdWordsLinks")
  
})

test_that("Get Adwords", {
  skip_on_cran()
  al <- ga_adwords(accountId, 
                   webPropertyId = webPropId, 
                   webPropertyAdWordsLinkId = "34H8JW1_R4K3Nh4uZpsIvw")
  
  expect_equal(al$kind, "analytics#entityAdWordsLink")
  
})

context("Custom data source")

test_that("Custom data source list",{
  skip_on_cran()
  ds <- ga_custom_datasource(accountId2, webPropId2)
  
  expect_equal(ds$kind, "analytics#customDataSource")
})

test_that("Custom data source upload list",{
  skip_on_cran()
  ds <- ga_custom_upload_list(accountId2, 
                              webPropertyId = webPropId2, 
                              customDataSourceId = "_jDsJHSFSU-uw038Bh8fUg")
  
  expect_s3_class(ds, "data.frame")
})



context("Custom metrics download")

test_that("Can get list of custom metrics and dimensions", {
  skip_on_cran()
  gacl <- ga_custom_vars_list(accountId, webPropId, type = "customMetrics")
  gacl2 <- ga_custom_vars_list(accountId, webPropId, type = "customDimensions") 
  
  expect_equal(gacl$kind, "analytics#customMetrics")
  expect_equal(gacl2$kind, "analytics#customDimensions")
})

test_that("Can get specific custom dimension", {
  
  gacm <- ga_custom_vars(accountId, webPropId, type = "customDimensions", customId = "ga:dimension1") 
  
  expect_equal(gacm$kind,"analytics#customDimension")
})




context("Goals")

test_that("Can get a goal list", {
  skip_on_cran()
  goals <- ga_goal_list(accountId, webPropId, ga_id)
  
  expect_equal(goals$kind, "analytics#goals")
  
  
})


test_that("Can get a goal entry", {
  skip_on_cran()
  goal <- ga_goal(accountId, webPropId, ga_id, 1)
  
  expect_equal(goal$kind, "analytics#goal")
  
  
})

context("Experiments")

test_that("Can fetch experiment list", {
  skip_on_cran()
  exper <- ga_experiment_list(accountId, webPropId, ga_id)
  
  expect_equal(exper$kind, "analytics#experiments")
})

context("Users")

test_that("Can fetch users list", {
  skip_on_cran()
  
  accountId <- 54019251
  webPropId <- "UA-54019251-4"
  ga_id <- 106249469
  
  acc <- ga_users_list(accountId)
  web <- ga_users_list(accountId, webPropertyId = webPropId)
  view <- ga_users_list(accountId, webPropertyId = webPropId, viewId = ga_id)
  
  expect_equal(acc$kind, "analytics#entityUserLinks")
  expect_equal(web$kind, "analytics#entityUserLinks")
  expect_equal(view$kind, "analytics#entityUserLinks")
  
})

## I have no experiment to fetch....
# test_that("Can fetch experiment", {
#   skip_on_cran()
#   exper <- ga_experiment(accountId, webPropId, ga_id, expId)
#   
#   expect_equal(exper, "analytics#experiment")
# })

# context("Remarketing")
# 
# test_that("List remarketing", {
#   
# })
# 
# test_that("Get a remarketing object", {
#   
# })

context("Allow metrics and dimensions")

test_that("Allowed metrics call", {
  skip_on_cran()
  
  m1 <- allowed_metric_dim("METRIC")
  m2 <- allowed_metric_dim("METRIC", "segment")
  m3 <- allowed_metric_dim("METRIC", "cohort")
  d1 <- allowed_metric_dim("DIMENSION")
  d2 <- allowed_metric_dim("DIMENSION", "segment")
  d3 <- allowed_metric_dim("DIMENSION", "cohort")
  
  expect_named(m1)
  expect_named(m2)
  expect_named(m3)
  expect_named(d1)
  expect_named(d2)
  expect_named(d3)
  
  expect_type(m1, "character")
  expect_type(m2, "character")
  expect_type(m3, "character")
  expect_type(d1, "character")
  expect_type(d2, "character")
  expect_type(d3, "character")
  
})
