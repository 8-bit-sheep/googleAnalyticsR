source("setup.R")

  context("Normal Data fetching")
  
  test_that("Vanilla test data fetch", {
    skip_on_cran()
    skip_on_travis()
    t11 <-   google_analytics(ga_id, 
                                date_range = c("2015-07-30","2015-10-01"),
                                dimensions=c('medium'), 
                                metrics = c('sessions'),
                                order = order_type("sessions"))  
    expect_s3_class(t11, "data.frame")
  })
  
  test_that("With custom date ranges", {
    skip_on_cran()
    skip_on_travis()
    t11 <-   google_analytics(ga_id, 
                              date_range = c("5DaysAgo","yesterday"),
                              dimensions=c('medium'), 
                              metrics = c('sessions'),
                              order = order_type("sessions"))  
    expect_s3_class(t11, "data.frame")
    
    t12 <- google_analytics(ga_id, 
                     date_range = c("15DaysAgo","today"),
                     dimensions=c('medium'), 
                     metrics = c('sessions'),
                     order = order_type("sessions"))  
    
    expect_s3_class(t11, "data.frame")
    
  })
  
  
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
  
  
  test_that("v3 Multi-channel funnels", {
    skip_on_cran()
    skip_on_travis()
    v3 <- google_analytics_3(ga_id, 
                           start = "2015-07-30", end = "2015-10-01",
                           dimensions=c('sourcePath'), 
                           metrics = c('totalConversions'),
                           type = "mcf")  
    
    expect_s3_class(v3, "data.frame")
    
  })
  
  
  context("Anti-sampling")

  test_that("Anti-sample when no sampling there", {
    skip_on_cran()
    skip_on_travis()
    as <-   google_analytics(ga_id,
                              date_range = c("2015-07-30","2016-10-01"),
                              dimensions=c('minute','hour','landingPagePath','medium','eventLabel','campaign'),
                              metrics = c('sessions'),
                              anti_sample = TRUE)
    expect_s3_class(as, "data.frame")


  })
  
  test_that("Aggregation works", {
    skip_on_cran()
    skip_on_travis()
    as <-   google_analytics(ga_id, 
                               date_range = c("2015-07-30","2016-10-01"),
                               dimensions=c('minute','hour','landingPagePath',
                                            'medium','eventLabel','campaign'), 
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
    skip_on_travis()
    ## first make a cohort group
    cohort4 <- make_cohort_group(list("Jan2016" = c("2016-01-01", "2016-01-31"), 
                                      "Feb2016" = c("2016-02-01","2016-02-28")))
    
    ## then call cohort report.  No date_range and must include metrics and dimensions
    ##   from the cohort list
    cohort_example <- google_analytics(ga_id, 
                                         dimensions=c('cohort'), 
                                         cohorts = cohort4, 
                                         metrics = c('cohortTotalUsers'))
    
    expect_s3_class(cohort_example, "data.frame")
    
  })
  
  
  test_that("Cohorts work with segments", {
    skip_on_cran()
    skip_on_travis()
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
    cohort_example <- google_analytics(ga_id, 
                                         dimensions=c('cohort'), 
                                         cohorts = cohort4, 
                                         segments = segment4,
                                         metrics = c('cohortTotalUsers'))
    
    expect_s3_class(cohort_example, "data.frame")
    
  })
  
  
  context("Pivots")
  
  test_that("Pivots work", {
    skip_on_cran()
    skip_on_travis()
    
    ## filter pivot results to 
    pivot_dim_filter1 <- dim_filter("source",
                                    "REGEXP",
                                    "organic|social|email|cpc")
    
    pivot_dim_clause <- filter_clause_ga4(list(pivot_dim_filter1))
    
    pivme <- pivot_ga4("source",
                       metrics = c("sessions"), 
                       maxGroupCount = 4, 
                       dim_filter_clause = pivot_dim_clause)
    
    pivtest1 <- google_analytics(ga_id, 
                                   c("2016-01-30","2016-10-01"), 
                                   dimensions=c('source'), 
                                   metrics = c('sessions'), 
                                   pivots = list(pivme))
    
    expect_s3_class(pivtest1, "data.frame")
    
  })
  
  
  context("Segments")
  
  test_that("Fetch segment list", {
    skip_on_cran()
    skip_on_travis()
    ## get list of segments
    my_segments <- ga_segment_list()
    
    expect_s3_class(my_segments, "data.frame")
    
  })
  
  
  test_that("Segment v3 syntax work", {
    skip_on_cran()
    skip_on_travis()
    ## choose the v3 segment
    segment_for_call <- "gaid::-4"
    
    ## make the v3 segment object in the v4 segment object:
    seg_obj <- segment_ga4("PaidTraffic", segment_id = segment_for_call)
    
    ## make the segment call
    segmented_ga1 <- google_analytics(ga_id, 
                                        c("2015-07-30","2015-10-01"), 
                                        dimensions=c('source','medium'), 
                                        segments = seg_obj, 
                                        metrics = c('sessions','bounces')
    )
    
    
    expect_s3_class(segmented_ga1, "data.frame")
    
    ## or pass the segment v3 defintion in directly:
    segment_def_for_call <- 
      "sessions::condition::ga:medium=~^(cpc|ppc|cpa|cpm|cpv|cpp)$"
    
    ## make the v3 segment object in the v4 segment object:
    seg_obj <- segment_ga4("PaidTraffic", segment_id = segment_def_for_call)
    
    ## make the segment call
    segmented_ga1 <- google_analytics(ga_id, 
                                        c("2015-07-30","2015-10-01"), 
                                        dimensions=c('source','medium','segment'), 
                                        segments = seg_obj, 
                                        metrics = c('sessions','bounces')
    )
    
    
    expect_s3_class(segmented_ga1, "data.frame")
    
  })
  
  
  test_that("Segment v4 syntax works - simple", {
    skip_on_cran()
    skip_on_travis()
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
    segment_example <- google_analytics(ga_id, 
                                          c("2015-07-30","2015-10-01"), 
                                          dimensions=c('source','medium','segment'), 
                                          segments = segment4, 
                                          metrics = c('sessions','bounces'))
    
    expect_s3_class(segment_example, "data.frame")
    
    
    
  })
  
  
  test_that("Segment v4 Syntax - step sequence", {
    skip_on_cran()
    skip_on_travis()
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
    segment_seq_example <- google_analytics(ga_id, 
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
