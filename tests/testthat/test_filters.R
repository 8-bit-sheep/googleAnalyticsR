source("setup.R")

context("Filters API Mocking")

test_that("Record requests if online", {
  skip_if_disconnected()
  googleAuthR::skip_if_no_env_auth(local_auth)

  ## test reqs
  capture_requests(
    {

      ## create filters on metrics
      mf <- met_filter("bounces", "GREATER_THAN", 0)
      mf2 <- met_filter("sessions", "GREATER", 2)

      ## create filters on dimensions
      df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
      df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)

      ## construct filter objects
      fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
      fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")

      google_analytics(ga_id,
                         date_range = c("2015-07-30","2015-10-01"),
                         dimensions=c('source','medium'),
                         metrics = c('sessions','bounces'),
                         met_filters = fc,
                         dim_filters = fc2,
                         filtersExpression = "ga:source!=(direct)")


      ga_filter_view_list(accountId, webPropId, ga_id)

      ga_filter_list(accountId)

      ga_filter(accountId, "22248057")

      ga_filter_view(accountId, webPropId, ga_id, "106249469:22248057")

    })



})


with_mock_API({
  context("Filters")

  test_that("Filter v4 GA fetches work", {
    skip_on_cran()
    ## create filters on metrics
    mf <- met_filter("bounces", "GREATER_THAN", 0)
    mf2 <- met_filter("sessions", "GREATER", 2)

    ## create filters on dimensions
    df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
    df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)

    ## construct filter objects
    fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
    fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")

    out <- google_analytics(ga_id,
                              date_range = c("2015-07-30","2015-10-01"),
                              dimensions=c('source','medium'),
                              metrics = c('sessions','bounces'),
                              met_filters = fc,
                              dim_filters = fc2,
                              filtersExpression = "ga:source!=(direct)")

    expect_s3_class(out, "data.frame")
    expect_gt(min(out$bounces), 0)
    expect_gt(min(out$sessions), 2)

    source_beginnings <- unique(vapply(out$source, function(x) substr(x, 1, 1), character(1)))
    expect_false("1" %in% source_beginnings)
    expect_false("a" %in% source_beginnings)

  })

  test_that("Get Filter View list", {
    skip_on_cran()

    fits <- ga_filter_view_list(accountId, webPropId, ga_id)

    expect_equal(fits$kind, "analytics#profileFilterLinks")

  })

  test_that("Get Filter list for account", {
    skip_on_cran()

    fits <- ga_filter_list(accountId)

    expect_equal(fits$kind, "analytics#filters")

  })

  test_that("Get Specific Filter", {
    skip_on_cran()

    fits <- ga_filter(accountId, "22248057")

    expect_equal(fits$kind, "analytics#filter")

  })

  test_that("Get Filter View", {
    skip_on_cran()

    fits <- ga_filter_view(accountId, webPropId, ga_id, "106249469:22248057")

    expect_equal(fits$kind, "analytics#profileFilterLink")

  })

  test_that("Delete Filter from account", {
      skip_on_cran()

      expect_warning(ga_filter_delete(accountId, filterId = filterId), "No JSON content detected")

  })

  test_that("Delete Filter from view", {
      skip_on_cran()

      expect_warning(ga_filter_delete(accountId, webPropId, viewId, filterId = ga_id, removeFromView = TRUE),
                     "No JSON content detected")

  })

  test_that("Add filter to the view", {
      skip_on_cran()

      response <- ga_filter_add(Filter, accountId, webPropId, ga_id, linkFilter = TRUE)

      expect_equal(response$kind, "analytics#profileFilterLink")

  })

  test_that("Add filter to the account", {
      skip_on_cran()

      response <- ga_filter_add(Filter, accountId)

      expect_equal(response, NULL)

  })

  test_that("Update existing filter", {
      skip_on_cran()

      response <- ga_filter_update(filter_to_update, accountId, filterId)

      expect_equal(response$kind, "analytics#filter")

  })

  test_that("Update view filter link", {
      skip_on_cran()

      response <- ga_filter_update_filter_link(viewFilterLink, accountId, webPropId, ga_id, linkId)

      expect_equal(response$kind, "analytics#profileFilterLink")

  })

  test_that("Apply existing filter to view", {
      skip_on_cran()

      response <- ga_filter_apply_to_view(filterId, accountId, webPropId, ga_id)

      expect_equal(response$kind, "analytics#profileFilterLink")

  })

})
