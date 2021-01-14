source("setup.R")

test_that("Basic fetch", {
  skip_on_cran()
  skip_on_travis()
  
  df <- ga_data(
    ga4_propertyId,
    metrics = "activeUsers",
    dimensions = c("date","city"),
    date_range = c("2020-03-31", "2020-04-27"),
    limit = 100
  )
  
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("date","city","activeUsers"))
  expect_true(inherits(df$activeUsers, "numeric"))
  expect_true(inherits(df$date, "Date"))
  expect_true(inherits(df$city, "character"))
  expect_snapshot_output(df)
  
})

test_that("Raw Data fetch", {
  skip_on_cran()
  skip_on_travis()
  
  the_json <- '{"entity":{"propertyId":"206670707"},"metrics":[{"name":"sessions"}],"orderBys":[{"dimension":{"orderType":"ALPHANUMERIC","dimensionName":"date"},"desc":false}],"dimensions":[{"name":"date"}],"dateRanges":[{"startDate":"2020-01-08","endDate":"2021-01-07"}],"keepEmptyRows":true,"limit":-1,"returnPropertyQuota":true}'
  
  raw <- ga_data(raw_json = the_json)
  expect_s3_class(raw, "data.frame")
  expect_snapshot_output(raw)
  
  raw_2 <- ga_data(raw_json = jsonlite::fromJSON(the_json))
  expect_s3_class(raw_2, "data.frame")
  expect_snapshot_output(raw_2)
  
  raw_rt <- ga_data(
    propertyId = ga4_propertyId, 
    raw_json = '{"metrics":[{"name":"activeUsers"}],"limit":100,"returnPropertyQuota":true}',
    realtime = TRUE)
  expect_s3_class(raw_rt, "data.frame")
  
})

test_that("Meta Data API",{
  skip_on_cran()
  skip_on_travis()
  
  meta44 <- ga_meta("data", propertyId =ga4_propertyId)
  expect_equal(meta44[meta44$apiName == "customEvent:test_dim","uiName"],
               "test_dim")
  expect_s3_class(meta44, "data.frame")
})

test_that("Filter objects", {

  string_f <- ga_aw_filter("city","Copenhagen","EXACT", caseSensitive = FALSE)
  in_list_f <- ga_aw_filter("city",c("Copenhagen","London"))
  numeric_f <- ga_aw_filter("activeUsers", 1L, "GREATER_THAN")
  numeric_list_f <- ga_aw_filter("activeUsers", c(1L,3L))
  no_nulls <- ga_aw_filter("city", NULL)
  
  expect_s3_class(string_f, "gar_Filter")
  expect_s3_class(string_f$stringFilter, "gar_StringFilter")
  expect_snapshot_output(string_f)
  
  expect_s3_class(in_list_f, "gar_Filter")
  expect_s3_class(in_list_f$inListFilter, "gar_InListFilter")
  expect_snapshot_output(in_list_f)
  
  expect_s3_class(numeric_f, "gar_Filter")
  expect_s3_class(numeric_f$numericFilter, "gar_NumericFilter")
  expect_snapshot_output(numeric_f)
  
  expect_s3_class(numeric_list_f, "gar_Filter")
  expect_s3_class(numeric_list_f$betweenFilter, "gar_BetweenFilter")
  expect_snapshot_output(numeric_list_f)
  
  expect_s3_class(no_nulls, "gar_Filter")
  expect_true(no_nulls$nullFilter)
  expect_snapshot_output(no_nulls)
  
})

test_that("Metric Aggregations", {
  skip_on_cran()
  skip_on_travis()
  ma <- ga_data(
    ga4_propertyId,
    metrics = c("activeUsers","sessions"),
    dimensions = c("date","city","dayOfWeek"),
    date_range = c("2020-03-31", "2020-04-27","2020-04-28", "2020-05-27"),
    metricAggregations = c("TOTAL","MAXIMUM","MINIMUM"),
    limit = 100
  )
  
  extract <- ga_data_aggregations(ma)
  ex1 <- ga_data_aggregations(ma, type = "maximums")
  
  expect_true(!is.null(attr(ma, "metricAggregations")$totals))
  # not guaranteed order of results, can't order due to API bug
  # expect_snapshot_output(ma) 
  
  expect_equal(names(extract), c("totals","maximums","minimums"))
  # expect_snapshot_output(extract)
  
  expect_true(is.data.frame(ex1))
  # expect_snapshot_output(ex1)
  
})

test_that("Custom data", {
  skip_on_cran()
  skip_on_travis()
  # create your own named metrics
  met_expression <- ga_data(
    ga4_propertyId,
    metrics = c("activeUsers","sessions",sessionsPerUser = "sessions/activeUsers"),
    dimensions = c("date","city","dayOfWeek"),
    date_range = c("2020-03-31", "2020-04-27"),
    limit = 100
  )
  
  expect_true("sessionsPerUser" %in% names(met_expression))
  expect_snapshot_output(met_expression)
  
  # create your own aggregation dimensions
  dim_expression <- ga_data(
    ga4_propertyId,
    metrics = c("activeUsers","sessions"),
    dimensions = c("date","city","dayOfWeek", cdow = "city/dayOfWeek"),
    date_range = c("2020-03-31", "2020-04-27"),
    limit = 100
  )
  
  expect_true("cdow" %in% names(dim_expression))
  expect_snapshot_output(dim_expression)
})

test_that("Ordering DSL objects", {
  
  o1 <- ga_data_order(-sessions)
  expect_s3_class(o1[[1]], "gar_OrderBy")
  expect_true(o1[[1]]$metric$metricName == "sessions")
  expect_true(o1[[1]]$desc)
  expect_snapshot_output(o1)
  
  o2 <- ga_data_order(+city)
  expect_true(o2[[1]]$dimension$dimensionName == "city")
  expect_false(o2[[1]]$desc)
  expect_snapshot_output(o2)
  
  o3 <- ga_data_order(+city -sessions)
  expect_true(o3[[1]]$dimension$dimensionName == "city")
  expect_false(o3[[1]]$desc)
  expect_true(o3[[2]]$metric$metricName == "sessions")
  expect_true(o3[[2]]$desc)
  expect_snapshot_output(o3)
  
  o4 <- ga_data_order(+city -sessions +activeUsers)
  expect_true(o4[[3]]$metric$metricName == "activeUsers")
  expect_false(o4[[3]]$desc)
  expect_snapshot_output(o4)

  o5 <- ga_data_order(+dayOfWeek, type = "NUMERIC")
  expect_true(o5[[1]]$dimension$dimensionName == "dayOfWeek")
  expect_false(o5[[1]]$desc)
  expect_snapshot_output(o5)
  
  o6 <- c(o1, o2)
  expect_s3_class(o6[[1]], "gar_OrderBy")
  expect_true(o6[[1]]$metric$metricName == "sessions")
  expect_true(o6[[1]]$desc)
  expect_true(o6[[2]]$dimension$dimensionName == "city")
  expect_false(o6[[2]]$desc)
  expect_snapshot_output(o6)
  
  
})

test_that("Order API fetch", {
  skip_on_cran()
  skip_on_travis()
  
  order<- ga_data(
    ga4_propertyId,
    metrics = c("activeUsers","sessions"),
    dimensions = c("date","city","dayOfWeek"),
    date_range = c("2020-03-31", "2020-04-27"),
    orderBys = ga_data_order(-sessions -dayOfWeek)
  )
  
  expect_true(is.data.frame(order))
  expect_true(order$sessions[[1]] > order$sessions[[2]])
  expect_snapshot_output(order)
})

test_that("Realtime", {
  skip_on_cran()
  skip_on_travis()
  realtime <- ga_data(
    ga4_propertyId,
    metrics = "activeUsers",
    dimensions = c("city","unifiedScreenName"),
    limit = 100,
    realtime = TRUE)
  
  expect_true(is.data.frame(realtime))
  
})

test_that("Filter fetch types", {
  skip_on_cran()
  skip_on_travis()
  
  test_filter <- function(dim_filter = NULL,
                          met_filter = NULL){
    ga_data(
      ga4_propertyId,
      metrics = "activeUsers",
      dimensions = c("date","city", "dayOfWeek"),
      date_range = c("2020-03-31", "2020-04-27"),
      dimensionFilter = dim_filter,
      metricFilter = met_filter,
      limit = 100
    )
  }
  
  #dimension filter
  string_f <- ga_data_filter(city=="Copenhagen")
  string_data <- test_filter(string_f)
  expect_equal(unique(string_data$city), "Copenhagen")
  expect_snapshot_output(string_data)
  
  in_list_f <- ga_data_filter(city==c("Copenhagen","London"))
  in_list_data <- test_filter(in_list_f)
  expect_true(all(unique(in_list_data$city) %in% c("London", "Copenhagen")))
  expect_snapshot_output(in_list_data)
  
  # metric filters
  numeric_f <- ga_data_filter(activeUsers>2)
  numeric_data <- test_filter(met_filter = numeric_f)
  expect_true(all(numeric_data$activeUsers >2))
  expect_snapshot_output(numeric_data)
  
  numeric_list_f <- ga_data_filter(activeUsers==c(2,6))
  numeric_list_data <- test_filter(met_filter = numeric_list_f)
  expect_true(all(numeric_list_data$activeUsers %in% 2:6))
  expect_snapshot_output(numeric_list_data)
  
})

