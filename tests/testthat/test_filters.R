source("setup.R")

context("Filters API Mocking")

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
    
    df3 <- dim_filter("medium","EXACT","organic")
    fc3 <- filter_clause_ga4(list(df3))

    # list of dim_filter clauses
    ga_data1 <- google_analytics(ga_id,
                                 date_range = c("2015-07-30","2015-10-01"),
                                 dimensions=c('source','medium'),
                                 metrics = c('sessions','bounces'),
                                 met_filters = fc,
                                 dim_filters = list(fc2,fc3),
                                 filtersExpression = "ga:source!=(direct)")
    
    expect_s3_class(ga_data1, "data.frame")

    expect_s3_class(out, "data.frame")
    expect_gt(min(out$bounces), 0)
    expect_gt(min(out$sessions), 2)

    source_beginnings <- unique(vapply(out$source, 
                                       function(x) substr(x, 1, 1), 
                                       character(1)))
    expect_false("1" %in% source_beginnings)
    expect_false("a" %in% source_beginnings)

  })

