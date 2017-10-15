library(httptest)
.mockPaths("..")

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics",
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

context("API Mocking")

test_that("Record requests if online", {
  skip_if_disconnected()
  skip_if_no_env_auth(local_auth)
  
  ## test reqs
  capture_requests(
    {
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
      google_analytics_4(ga_id, 
                                  date_range = c("2015-07-30","2015-10-01"),
                                  dimensions=c('medium'), 
                                  metrics = c('sessions'),
                                  order = order_type("sessions"))
    
    google_analytics_4(ga_id, 
                                date_range = c("2015-07-30","2015-12-31"),
                                dimensions=c('medium','source','hour','minute','campaign','pagePath'), 
                                metrics = c('sessions'),
                                max = -1)  

    google_analytics_4(ga_id, 
                                date_range = c("2015-07-30","2015-09-01"),
                                dimensions=c('medium','source','hour','minute','campaign','pagePath'), 
                                metrics = c('sessions'),
                                max = -1,
                                slow_fetch = TRUE)  

    # google_analytics(c(ga_id, ga_id2),
    #                           start = "2015-07-31", end = "2015-10-01",
    #                           dimensions=c('medium'), 
    #                           metrics = c('sessions'),
    #                           sort = "ga:sessions")
    
    # google_analytics(c(ga_id2, ga_id),
    #                           start = "2015-07-30", end = "2015-10-01",
    #                           dimensions=c('medium'), 
    #                           metrics = c('sessions'),
    #                           sort = "ga:sessions",
    #                           multi_account_batching = TRUE)
    
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
    google_analytics_4(ga_id, 
                              date_range = c("2015-07-30","2015-10-01"),
                              dimensions=c('medium'), 
                              metrics = c('sessions'),
                              order = order_type("sessions")) 

    google_analytics(ga_id, 
                           start = "2015-07-30", end = "2015-10-01",
                           dimensions=c('sourcePath'), 
                           metrics = c('totalConversions'),
                           type = "mcf")  

    ## create filters on metrics
    mf <- met_filter("bounces", "GREATER_THAN", 0)
    mf2 <- met_filter("sessions", "GREATER", 2)
    
    ## create filters on dimensions
    df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
    df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)
    
    ## construct filter objects
    fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
    fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")
    
    out <- google_analytics_4(ga_id, 
                              date_range = c("2015-07-30","2015-10-01"),
                              dimensions=c('source','medium'), 
                              metrics = c('sessions','bounces'), 
                              met_filters = fc, 
                              dim_filters = fc2, 
                              filtersExpression = "ga:source!=(direct)")

    
    fits <- ga_filter_view_list(accountId, webPropId, ga_id)
    
    fits <- ga_filter_list(accountId)
    
    fits <- ga_filter(accountId, "22248057")
    
    fits <- ga_filter_view(accountId, webPropId, ga_id, "106249469:22248057")

    as <-   google_analytics_4(ga_id, 
                               date_range = c("2015-07-30","2016-10-01"),
                               dimensions=c('minute','hour','landingPagePath','medium','eventLabel','campaign'), 
                               metrics = c('sessions', 'avgSessionDuration'),
                               anti_sample = TRUE)  
    expect_s3_class(as, "data.frame")

    ## first make a cohort group
    cohort4 <- make_cohort_group(list("Jan2016" = c("2016-01-01", "2016-01-31"), 
                                      "Feb2016" = c("2016-02-01","2016-02-28")))
    
    ## then call cohort report.  No date_range and must include metrics and dimensions
    ##   from the cohort list
    cohort_example <- google_analytics_4(ga_id, 
                                         dimensions=c('cohort'), 
                                         cohorts = cohort4, 
                                         metrics = c('cohortTotalUsers'))

    ## first make a cohort group
    cohort4 <- make_cohort_group(list("Jan2016" = c("2016-01-01", "2016-01-31"), 
                                      "Feb2016" = c("2016-02-01","2016-02-28")))
    ## make two segment elements
    se <- segment_element("sessions", 
                          operator = "GREATER_THAN", 
                          type = "METRIC", 
                          comparisonValue = 1, 
                          scope = "USER")
    
    se2 <- segment_element("medium", 
                           operator = "EXACT", 
                           type = "DIMENSION", 
                           expressions = "organic")
    
    ## choose between segment_vector_simple or segment_vector_sequence
    ## Elements can be combined into clauses, which can then be combined into OR filter clauses
    sv_simple <- segment_vector_simple(list(list(se)))
    
    sv_simple2 <- segment_vector_simple(list(list(se2)))
    
    ## Each segment vector can then be combined into a logical AND
    seg_defined <- segment_define(list(sv_simple, sv_simple2))
    
    ## Each segement defintion can apply to users, sessions or both.
    ## You can pass a list of several segments
    segment4 <- segment_ga4("simple", user_segment = seg_defined)
    ## then call cohort report.  No date_range and must include metrics and dimensions
    ##   from the cohort list
    cohort_example <- google_analytics_4(ga_id, 
                                         dimensions=c('cohort'), 
                                         cohorts = cohort4, 
                                         segments = segment4,
                                         metrics = c('cohortTotalUsers'))

    
    ## filter pivot results to 
    pivot_dim_filter1 <- dim_filter("source",
                                    "REGEXP",
                                    "organic|social|email|cpc")
    
    pivot_dim_clause <- filter_clause_ga4(list(pivot_dim_filter1))
    
    pivme <- pivot_ga4("source",
                       metrics = c("sessions"), 
                       maxGroupCount = 4, 
                       dim_filter_clause = pivot_dim_clause)
    
    pivtest1 <- google_analytics_4(ga_id, 
                                   c("2016-01-30","2016-10-01"), 
                                   dimensions=c('source'), 
                                   metrics = c('sessions'), 
                                   pivots = list(pivme))

    ## get list of segments
    my_segments <- ga_segment_list()

  
    ## choose the v3 segment
    segment_for_call <- "gaid::-4"
    
    ## make the v3 segment object in the v4 segment object:
    seg_obj <- segment_ga4("PaidTraffic", segment_id = segment_for_call)
    
    ## make the segment call
    segmented_ga1 <- google_analytics_4(ga_id, 
                                        c("2015-07-30","2015-10-01"), 
                                        dimensions=c('source','medium'), 
                                        segments = seg_obj, 
                                        metrics = c('sessions','bounces'))

    
    ## or pass the segment v3 defintion in directly:
    segment_def_for_call <- "sessions::condition::ga:medium=~^(cpc|ppc|cpa|cpm|cpv|cpp)$"
    
    ## make the v3 segment object in the v4 segment object:
    seg_obj <- segment_ga4("PaidTraffic", segment_id = segment_def_for_call)
    
    ## make the segment call
    segmented_ga1 <- google_analytics_4(ga_id, 
                                        c("2015-07-30","2015-10-01"), 
                                        dimensions=c('source','medium','segment'), 
                                        segments = seg_obj, 
                                        metrics = c('sessions','bounces'))

    se <- segment_element("sessions", 
                          operator = "GREATER_THAN", 
                          type = "METRIC", 
                          comparisonValue = 1, 
                          scope = "USER")
    
    se2 <- segment_element("medium", 
                           operator = "EXACT", 
                           type = "DIMENSION", 
                           expressions = "organic")
    
    ## choose between segment_vector_simple or segment_vector_sequence
    ## Elements can be combined into clauses, which can then be combined into OR filter clauses
    sv_simple <- segment_vector_simple(list(list(se)))
    
    sv_simple2 <- segment_vector_simple(list(list(se2)))
    
    ## Each segment vector can then be combined into a logical AND
    seg_defined <- segment_define(list(sv_simple, sv_simple2))
    
    ## Each segement defintion can apply to users, sessions or both.
    ## You can pass a list of several segments
    segment4 <- segment_ga4("simple", user_segment = seg_defined)
    
    ## Add the segments to the segments param
    segment_example <- google_analytics_4(ga_id, 
                                          c("2015-07-30","2015-10-01"), 
                                          dimensions=c('source','medium','segment'), 
                                          segments = segment4, 
                                          metrics = c('sessions','bounces'))
    
    se2 <- segment_element("medium", 
                           operator = "EXACT", 
                           type = "DIMENSION", 
                           expressions = "organic")
    
    se3 <- segment_element("medium",
                           operator = "EXACT",
                           type = "DIMENSION",
                           not = TRUE,
                           expressions = "organic")
    
    ## step sequence
    ## users who arrived via organic then via referral
    sv_sequence <- segment_vector_sequence(list(list(se2), 
                                                list(se3)))
    
    seq_defined2 <- segment_define(list(sv_sequence))
    
    segment4_seq <- segment_ga4("sequence", user_segment = seq_defined2)
    
    ## Add the segments to the segments param
    segment_seq_example <- google_analytics_4(ga_id, 
                                              c("2016-01-01","2016-03-01"), 
                                              dimensions=c('source','segment'), 
                                              segments = segment4_seq,
                                              metrics = c('sessions','bounces'))
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

with_mock_API({
  
  

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


context("Normal Data fetching")

test_that("Download meta data", {
  skip_on_cran()
  meta <- google_analytics_meta()
  
  expect_s3_class(meta, "data.frame")
  
})

test_that("Vanilla test data fetch", {
  skip_on_cran()
  t11 <-   google_analytics_4(ga_id, 
                              date_range = c("2015-07-30","2015-10-01"),
                              dimensions=c('medium'), 
                              metrics = c('sessions'),
                              order = order_type("sessions"))  
  expect_s3_class(t11, "data.frame")
})

test_that("Big v4 batch", {
  skip_on_cran()
  big <-   google_analytics_4(ga_id, 
                              date_range = c("2015-07-30","2015-12-31"),
                              dimensions=c('medium','source','hour','minute','campaign','pagePath'), 
                              metrics = c('sessions'),
                              max = -1)  
  expect_s3_class(big, "data.frame")
})

test_that("Slow big v4 batch", {
  skip_on_cran()
  big <-   google_analytics_4(ga_id, 
                              date_range = c("2015-07-30","2015-09-01"),
                              dimensions=c('medium','source','hour','minute','campaign','pagePath'), 
                              metrics = c('sessions'),
                              max = -1,
                              slow_fetch = TRUE)  
  expect_s3_class(big, "data.frame")
})

# test_that("v3 multi account batching without flag", {
#   
#   skip_on_cran()
#   multi <- google_analytics(c(ga_id, ga_id2),
#                             start = "2015-07-31", end = "2015-10-01",
#                             dimensions=c('medium'), 
#                             metrics = c('sessions'),
#                             sort = "ga:sessions")
#   
#   expect_length(multi, 2)
#   
#   expect_s3_class(multi[[1]], "data.frame")
#   expect_s3_class(multi[[2]], "data.frame")  
#   
# })
# 
# test_that("v3 multi account batching with flag", {
#   skip_on_cran()
#   
#   multi <- google_analytics(c(ga_id2, ga_id),
#                             start = "2015-07-30", end = "2015-10-01",
#                             dimensions=c('medium'), 
#                             metrics = c('sessions'),
#                             sort = "ga:sessions",
#                             multi_account_batching = TRUE)
#   
#   expect_length(multi, 2)
#   
#   expect_s3_class(multi[[1]], "data.frame")
#   expect_s3_class(multi[[2]], "data.frame")  
#   
# })
# 
# test_that("v3 WALK data", {
#   skip_on_cran()
#   
#   walked <- suppressWarnings(
#     google_analytics(ga_id,
#                      start = "2015-07-30", end = "2015-09-01",
#                      dimensions=c('medium'), 
#                      metrics = c('sessions'),
#                      sort = "ga:sessions",
#                      samplingLevel = "WALK"))
#   
#   expect_s3_class(walked, "data.frame")  
#   
# })
# 
# test_that("v3 Batch data", {
#   skip_on_cran()
#   
#   bb <- google_analytics(ga_id,
#                              start = "2015-07-30", end = "2015-10-01",
#                              dimensions=c('medium','source','hour','minute','pagePath'), 
#                              metrics = c('sessions'),
#                              sort = "ga:sessions",
#                              max_results = 30000)
#   
#   expect_s3_class(bb, "data.frame")  
#   
# })
# 
# test_that("v4 API matches v3 equivalent API call", {
#   skip_on_cran()
#   v3 <- google_analytics(ga_id, 
#                          start = "2015-07-30", end = "2015-10-01",
#                          dimensions=c('medium'), 
#                          metrics = c('sessions'),
#                          sort = "ga:sessions")  
#   v4 <-  google_analytics_4(ga_id, 
#                             date_range = c("2015-07-30","2015-10-01"),
#                             dimensions=c('medium'), 
#                             metrics = c('sessions'),
#                             order = order_type("sessions")) 
#   
#   expect_equal(v3$medium, v4$medium)
#   expect_equal(v3$sessions, v4$sessions)
#   
# })

test_that("v3 Multi-channel funnels", {
  skip_on_cran()
  v3 <- google_analytics(ga_id, 
                         start = "2015-07-30", end = "2015-10-01",
                         dimensions=c('sourcePath'), 
                         metrics = c('totalConversions'),
                         type = "mcf")  
  
  expect_s3_class(v3, "data.frame")
  
})

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
  
  out <- google_analytics_4(ga_id, 
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

# context("Anti-sampling")
# 
# test_that("Anti-sample when no sampling there", {
#   skip_on_cran()
#   as <-   google_analytics_4(ga_id, 
#                             date_range = c("2015-07-30","2016-10-01"),
#                             dimensions=c('minute','hour','landingPagePath','medium','eventLabel','campaign'), 
#                             metrics = c('sessions'),
#                             anti_sample = TRUE)  
#   expect_s3_class(as, "data.frame")
#   
#   
# })

test_that("Aggregation works", {
  skip_on_cran()
  as <-   google_analytics_4(ga_id, 
                             date_range = c("2015-07-30","2016-10-01"),
                             dimensions=c('minute','hour','landingPagePath','medium','eventLabel','campaign'), 
                             metrics = c('sessions', 'avgSessionDuration'),
                             anti_sample = TRUE)  
  expect_s3_class(as, "data.frame")
  
  agg <- aggregateGAData(as, agg_names = "medium")
  expect_equal(names(agg), c("medium", "sessions", "avgSessionDuration"))
  expect_lt(max(agg$avgSessionDuration), max(as$avgSessionDuration))
  
})

context("Cohorts")

test_that("Cohorts work", {
  skip_on_cran()
  ## first make a cohort group
  cohort4 <- make_cohort_group(list("Jan2016" = c("2016-01-01", "2016-01-31"), 
                                    "Feb2016" = c("2016-02-01","2016-02-28")))
  
  ## then call cohort report.  No date_range and must include metrics and dimensions
  ##   from the cohort list
  cohort_example <- google_analytics_4(ga_id, 
                                       dimensions=c('cohort'), 
                                       cohorts = cohort4, 
                                       metrics = c('cohortTotalUsers'))
  
  expect_s3_class(cohort_example, "data.frame")
  
})

test_that("Cohorts work with segments", {
  skip_on_cran()
  ## first make a cohort group
  cohort4 <- make_cohort_group(list("Jan2016" = c("2016-01-01", "2016-01-31"), 
                                    "Feb2016" = c("2016-02-01","2016-02-28")))
  ## make two segment elements
  se <- segment_element("sessions", 
                        operator = "GREATER_THAN", 
                        type = "METRIC", 
                        comparisonValue = 1, 
                        scope = "USER")
  
  se2 <- segment_element("medium", 
                         operator = "EXACT", 
                         type = "DIMENSION", 
                         expressions = "organic")
  
  ## choose between segment_vector_simple or segment_vector_sequence
  ## Elements can be combined into clauses, which can then be combined into OR filter clauses
  sv_simple <- segment_vector_simple(list(list(se)))
  
  sv_simple2 <- segment_vector_simple(list(list(se2)))
  
  ## Each segment vector can then be combined into a logical AND
  seg_defined <- segment_define(list(sv_simple, sv_simple2))
  
  ## Each segement defintion can apply to users, sessions or both.
  ## You can pass a list of several segments
  segment4 <- segment_ga4("simple", user_segment = seg_defined)
  ## then call cohort report.  No date_range and must include metrics and dimensions
  ##   from the cohort list
  cohort_example <- google_analytics_4(ga_id, 
                                       dimensions=c('cohort'), 
                                       cohorts = cohort4, 
                                       segments = segment4,
                                       metrics = c('cohortTotalUsers'))
  
  expect_s3_class(cohort_example, "data.frame")
  
})

context("Pivots")

test_that("Pivots work", {
  skip_on_cran()
  
  ## filter pivot results to 
  pivot_dim_filter1 <- dim_filter("source",
                                  "REGEXP",
                                  "organic|social|email|cpc")
  
  pivot_dim_clause <- filter_clause_ga4(list(pivot_dim_filter1))
  
  pivme <- pivot_ga4("source",
                     metrics = c("sessions"), 
                     maxGroupCount = 4, 
                     dim_filter_clause = pivot_dim_clause)
  
  pivtest1 <- google_analytics_4(ga_id, 
                                 c("2016-01-30","2016-10-01"), 
                                 dimensions=c('source'), 
                                 metrics = c('sessions'), 
                                 pivots = list(pivme))
  
  expect_s3_class(pivtest1, "data.frame")
  
})

context("Segments")

test_that("Fetch segment list", {
  skip_on_cran()
  ## get list of segments
  my_segments <- ga_segment_list()
  
  expect_equal(my_segments$kind, "analytics#segments")
  
})

test_that("Segment v3 syntax work", {
  skip_on_cran()
  ## choose the v3 segment
  segment_for_call <- "gaid::-4"
  
  ## make the v3 segment object in the v4 segment object:
  seg_obj <- segment_ga4("PaidTraffic", segment_id = segment_for_call)
  
  ## make the segment call
  segmented_ga1 <- google_analytics_4(ga_id, 
                                      c("2015-07-30","2015-10-01"), 
                                      dimensions=c('source','medium'), 
                                      segments = seg_obj, 
                                      metrics = c('sessions','bounces')
  )
  
  expect_s3_class(segmented_ga1, "data.frame")
  
  ## or pass the segment v3 defintion in directly:
  segment_def_for_call <- "sessions::condition::ga:medium=~^(cpc|ppc|cpa|cpm|cpv|cpp)$"
  
  ## make the v3 segment object in the v4 segment object:
  seg_obj <- segment_ga4("PaidTraffic", segment_id = segment_def_for_call)
  
  ## make the segment call
  segmented_ga1 <- google_analytics_4(ga_id, 
                                      c("2015-07-30","2015-10-01"), 
                                      dimensions=c('source','medium','segment'), 
                                      segments = seg_obj, 
                                      metrics = c('sessions','bounces')
  )
  
  expect_s3_class(segmented_ga1, "data.frame")
  
})

test_that("Segment v4 syntax works - simple", {
  skip_on_cran()
  ## make two segment elements
  se <- segment_element("sessions", 
                        operator = "GREATER_THAN", 
                        type = "METRIC", 
                        comparisonValue = 1, 
                        scope = "USER")
  
  se2 <- segment_element("medium", 
                         operator = "EXACT", 
                         type = "DIMENSION", 
                         expressions = "organic")
  
  ## choose between segment_vector_simple or segment_vector_sequence
  ## Elements can be combined into clauses, which can then be combined into OR filter clauses
  sv_simple <- segment_vector_simple(list(list(se)))
  
  sv_simple2 <- segment_vector_simple(list(list(se2)))
  
  ## Each segment vector can then be combined into a logical AND
  seg_defined <- segment_define(list(sv_simple, sv_simple2))
  
  ## Each segement defintion can apply to users, sessions or both.
  ## You can pass a list of several segments
  segment4 <- segment_ga4("simple", user_segment = seg_defined)
  
  ## Add the segments to the segments param
  segment_example <- google_analytics_4(ga_id, 
                                        c("2015-07-30","2015-10-01"), 
                                        dimensions=c('source','medium','segment'), 
                                        segments = segment4, 
                                        metrics = c('sessions','bounces'))
  
  expect_s3_class(segment_example, "data.frame")
  
  
  
})

test_that("Segment v4 Syntax - step sequence", {
  skip_on_cran()
  ## Sequence segment
  
  se2 <- segment_element("medium", 
                         operator = "EXACT", 
                         type = "DIMENSION", 
                         expressions = "organic")
  
  se3 <- segment_element("medium",
                         operator = "EXACT",
                         type = "DIMENSION",
                         not = TRUE,
                         expressions = "organic")
  
  ## step sequence
  ## users who arrived via organic then via referral
  sv_sequence <- segment_vector_sequence(list(list(se2), 
                                              list(se3)))
  
  seq_defined2 <- segment_define(list(sv_sequence))
  
  segment4_seq <- segment_ga4("sequence", user_segment = seq_defined2)
  
  ## Add the segments to the segments param
  segment_seq_example <- google_analytics_4(ga_id, 
                                            c("2016-01-01","2016-03-01"), 
                                            dimensions=c('source','segment'), 
                                            segments = segment4_seq,
                                            metrics = c('sessions','bounces'))
  
  
  expect_s3_class(segment_seq_example, "data.frame")
  
  
})

context("Upload")

# test_that("Can upload a data.frame ", {
#   skip_on_cran()
#   upload_me <- data.frame(medium = "shinyapps", 
#                           source = "referral", 
#                           adCost = 1, 
#                           date = "20160801")
#   
#   rr <- ga_custom_upload_file(47480439, "UA-47480439-2", "_jDsJHSFSU-uw038Bh8fUg", upload_me)
#   expect_equal(rr$kind, "analytics#upload")
#   
#   new_rr <- ga_custom_upload(upload_object = rr)
#   expect_equal(rr$kind, "analytics#upload")
# })

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

# context("BigQuery")
# 
# test_that("Can query from BigQuery using parser", {
#   skip_on_cran()
#   
#   bq_result <- google_analytics_bq("mark-edmondson-gde",
#                                    "98288890",
#                                    start = "2015-08-10", end = "2016-03-01",
#                                    metrics = c("sessions","users"),
#                                    dimensions = c("date","medium"))
#   
#   expect_s3_class(bq_result, "data.frame")
#   
# })
# 
# test_that("Can query from BigQuery directly",{
#   skip_on_cran()
#   
#   q <- "SELECT fullVisitorId, visitId, date, hits.hour as hour, hits.minute as minute FROM (TABLE_DATE_RANGE([98288890.ga_sessions_], TIMESTAMP('2016-08-10'), TIMESTAMP('2016-09-01'))) group by fullVisitorId, visitId, date, hour, minute LIMIT 10000"
#   
#   bq_result <- google_analytics_bq("mark-edmondson-gde",
#                                    "98288890",
#                                    query = q)
#   
#   expect_s3_class(bq_result, "data.frame")
#   
#   
# })

})