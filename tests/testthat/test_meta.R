source("setup.R")

context("Meta data")

test_that("Download meta data", {
  skip_on_cran()
  skip_on_travis()
  meta <- ga_meta()
  
  expect_s3_class(meta, "data.frame")
  
})


context("Accounts")

test_that("Get the account list", {
  skip_on_cran()
  skip_on_travis()
  al <- ga_accounts()
  
  expect_s3_class(al, "data.frame")
  
})



test_that("Get the account summary list", {
  skip_on_cran()
  skip_on_travis()
  al <- ga_account_list()
  expect_s3_class(al, "data.frame")
  
  cat("\nScopes:", getOption("googleAuthR.scopes.selected"))
  
  ga4 <- ga_account_list("ga4")
  expect_s3_class(ga4, "data.frame")
  expect_equal(names(ga4), 
               c("account_name","accountId","property_name","propertyId"))
  
})


context("Webproperties")

test_that("Get the webproperty list", {
  skip_on_cran()
  skip_on_travis()
  wb <- ga_webproperty_list(accountId)
  
  expect_s3_class(wb, "data.frame")
  
})

test_that("Get the web property", {
  skip_on_cran()
  skip_on_travis()
  wb <- ga_webproperty(accountId, "UA-54019251-1")
  
  expect_equal(wb$kind, "analytics#webproperty")
  
})

context("Views")

test_that("Get the view list", {
  skip_on_cran()
  skip_on_travis()
  views <- ga_view_list(accountId, webPropId)
  
  expect_s3_class(views, "data.frame")
  
})

test_that("Get the individual View", {
  skip_on_cran()
  skip_on_travis()
  wb <- ga_view(accountId, webPropertyId = webPropId, profileId = ga_id)
  
  expect_equal(wb$kind, "analytics#profile")
  
})

context("AdWords")

test_that("Get Adwords list", {
  skip_on_cran()
  skip_on_travis()
  al <- ga_adwords_list(accountId, webPropertyId = webPropId)
  
  expect_s3_class(al, "data.frame")
  
})

test_that("Get Adwords", {
  skip_on_cran()
  skip_on_travis()
  al <- ga_adwords(accountId, 
                   webPropertyId = webPropId, 
                   webPropertyAdWordsLinkId = "34H8JW1_R4K3Nh4uZpsIvw")
  
  expect_equal(al$kind, "analytics#entityAdWordsLink")
  
})

context("Custom data source")

test_that("Custom data source list",{
  skip_on_cran()
  skip_on_travis()
  ds <- ga_custom_datasource(accountId2, webPropId2)
  
  expect_equal(ds$kind, "analytics#customDataSource")
})

test_that("Custom data source upload list",{
  skip_on_cran()
  skip_on_travis()
  ds <- ga_custom_upload_list(accountId2, 
                              webPropertyId = webPropId2, 
                              customDataSourceId = "kvks-PO8SfG-yRcKjI00-g")
  
  expect_s3_class(ds, "data.frame")
})



context("Custom metrics download")

test_that("Can get list of custom metrics and dimensions", {
  skip_on_cran()
  skip_on_travis()
  gacl <- ga_custom_vars_list(accountId, webPropId, type = "customMetrics")
  gacl2 <- ga_custom_vars_list(accountId, webPropId, type = "customDimensions") 
  
  expect_s3_class(gacl, "data.frame")
  expect_s3_class(gacl2, "data.frame")
  
})

test_that("Can get specific custom dimension", {
  skip_on_cran()
  skip_on_travis()
  gacm <- ga_custom_vars(accountId, webPropId, type = "customDimensions", customId = "ga:dimension1") 
  
  expect_equal(gacm$kind,"analytics#customDimension")
})




context("Goals")

test_that("Can get a goal list", {
  skip_on_cran()
  skip_on_travis()
  goals <- ga_goal_list(accountId, webPropId, ga_id)
  
  expect_s3_class(goals, "data.frame")
  
  
})


test_that("Can get a goal entry", {
  skip_on_cran()
  skip_on_travis()
  goal <- ga_goal(accountId, webPropId, ga_id, 1)
  
  expect_equal(goal$kind, "analytics#goal")
  
  
})

context("Experiments")

test_that("Can fetch experiment list", {
  skip_on_cran()
  skip_on_travis()
  exper <- ga_experiment_list(accountId, webPropId, ga_id)
  
  expect_s3_class(exper, "data.frame")

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
  
  m1 <- ga_allowed_metric_dim("METRIC")
  m2 <- ga_allowed_metric_dim("METRIC", "segment")
  m3 <- ga_allowed_metric_dim("METRIC", "cohort")
  d1 <- ga_allowed_metric_dim("DIMENSION")
  d2 <- ga_allowed_metric_dim("DIMENSION", "segment")
  d3 <- ga_allowed_metric_dim("DIMENSION", "cohort")
  
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
