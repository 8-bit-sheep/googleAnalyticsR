source("setup.R")

context("Filter management")

test_that("Get Filter View list", {
  skip_on_cran()
  
  fits <- ga_filter_view_list(accountId = accountId, 
                              webPropertyId = webPropId, 
                              viewId = ga_id)
  
  expect_s3_class(fits, "data.frame")
  
})

test_that("Get Filter list for account", {
  skip_on_cran()
  
  fits <- ga_filter_list(accountId)
  
  expect_s3_class(fits, "data.frame")
  
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
  
  ## move rank to 1
  viewFilterLink <- list(rank = 1)
  
  response2 <- ga_filter_update_filter_link(viewFilterLink, 
                                            accountId = accountId2, 
                                            webPropertyId = webPropId2, 
                                            viewId = ga_id2, 
                                            linkId = response$id)
  expect_equal(response2$kind, "analytics#profileFilterLink")
  expect_equal(response2$rank, 1)
  
  del <- ga_filter_delete(accountId = accountId2, 
                          webPropertyId = webPropId2, 
                          viewId = ga_id2, 
                          filterId =response2$filterRef$id, 
                          removeFromView = TRUE)
  
  expect_true(del)
  
  
})

test_that("Add filter to the account, but not link", {
  skip_on_cran()
  
  Filter <- list(
    name = 'googleAnalyticsR test2: Exclude Internal Traffic',
    type = 'EXCLUDE',
    excludeDetails = list(
      field = 'GEO_IP_ADDRESS',
      matchType = 'EQUAL',
      expressionValue = '199.04.123.1',
      caseSensitive = 'False'
    )
  )
  
  filterId <- ga_filter_add(Filter, 
                            accountId = accountId2, 
                            linkFilter = FALSE)
  
  expect_type(filterId, "character")
  
  test_name <- "googleAnalyticsR test3: Changed name via PATCH"
  
  filter_to_update <- list(name = test_name)
  
  patched <- ga_filter_update(filter_to_update, accountId2, filterId, method = "PATCH")
  
  expect_equal(patched$kind, "analytics#filter")
  expect_equal(patched$name, test_name)
  
  # delete the filter
  del <- ga_filter_delete(accountId = accountId2, 
                          filterId = filterId)
  
  expect_true(del)
  
})
