source("setup.R")

context("Filter management")

test_that("Get Filter View list", {
  skip_on_cran()
  
  fits <- ga_filter_view_list(accountId = accountId, 
                              webPropertyId = webPropId, 
                              viewId = ga_id)
  
  expect_equal(fits$kind, "analytics#profileFilterLinks")
  
})

test_that("Get Filter list for account", {
  skip_on_cran()
  
  fits <- ga_filter_list(accountId)
  
  expect_equal(fits$kind, "analytics#filters")
  
})

test_that("Get Specific Filter", {
  skip_on_cran()
  
  fits <- ga_filter(accountId, filterId = "22248057")
  
  expect_equal(fits$kind, "analytics#filter")
  
})

test_that("Get Filter View", {
  skip_on_cran()
  
  fits <- ga_filter_view(accountId, 
                         webPropertyId = webPropId, 
                         viewId = ga_id, 
                         linkId = "106249469:22248057")
  
  expect_equal(fits$kind, "analytics#profileFilterLink")
  
})

# Thu Feb  8 12:56:55 2018 ------------------------------

test_that("Add filter to the view", {
  skip_on_cran()
  
  Filter <- list(
    name = 'googleAnalyticsR test: Exclude Internal Traffic',
    type = 'EXCLUDE',
    excludeDetails = list(
      field = 'GEO_IP_ADDRESS',
      matchType = 'EQUAL',
      expressionValue = '199.04.123.1',
      caseSensitive = 'False'
    )
  )
  
  response <- ga_filter_add(Filter, 
                            accountId = accountId2, 
                            webPropertyId = webPropId2, 
                            viewId = ga_id2, 
                            linkFilter = TRUE)
  
  expect_equal(response$kind, "analytics#profileFilterLink")
  
})

test_that("Add filter to the account", {
  skip_on_cran()
  
  response <- ga_filter_add(Filter, accountId2)
  
  expect_equal(response, NULL)
  
})

test_that("Update existing filter", {
  skip_on_cran()
  
  response <- ga_filter_update(filter_to_update, accountId2, filterId2)
  
  expect_equal(response$kind, "analytics#filter")
  
})

test_that("Update view filter link", {
  skip_on_cran()
  
  response <- ga_filter_update_filter_link(viewFilterLink, accountId2, webPropId2, ga_id2, linkId)
  
  expect_equal(response$kind, "analytics#profileFilterLink")
  
})

test_that("Apply existing filter to view", {
  skip_on_cran()
  
  response <- ga_filter_apply_to_view(filterId, accountId2, webPropId2, ga_id2)
  
  expect_equal(response$kind, "analytics#profileFilterLink")
  
})

test_that("Delete Filter from account", {
  skip_on_cran()
  
  expect_warning(ga_filter_delete(accountId2, filterId = filterId), 
                 "No JSON content detected")
  
})

test_that("Delete Filter from view", {
  skip_on_cran()
  
  expect_warning(ga_filter_delete(accountId2, webPropId2, viewId2, filterId = ga_id, removeFromView = TRUE),
                 "No JSON content detected")
  
})