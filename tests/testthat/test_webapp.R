context("App + Web Tests")

test_that("Basic fetch", {
  
  df <- google_analytics_aw(
    206670707,
    metrics = "activeUsers",
    dimensions = c("date","city"),
    date_range = c("2020-03-31", "2020-04-27"),
    limit = 100
  )
  
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("date","city","activeUsers"))
  
  city_dk <- ga_aw_filter_expr(
    ga_aw_filter("city", "Copenhagen", "EXACT")
  )
  
  expect_s3_class(city_dk, "gar_FilterExpression")
})

test_that("Filter objects", {
  
  string_f <- ga_aw_filter("city","Copenhagen","EXACT", caseSensitive = FALSE)
  in_list_f <- ga_aw_filter("city",c("Copenhagen","London"))
  numeric_f <- ga_aw_filter("activeUsers", 1L, "GREATER_THAN")
  # need to create metric expressions that are floats
  #float_f <- ga_aw_filter("")
  numeric_list_f <- ga_aw_filter("activeUsers", c(1L,3L))
  no_nulls <- ga_aw_filter("city", TRUE)
  
  expect_s3_class(string_f, "gar_Filter")
  expect_s3_class(string_f$stringFilter, "gar_StringFilter")
  
  expect_s3_class(in_list_f, "gar_Filter")
  expect_s3_class(in_list_f$inListFilter, "gar_InListFilter")
  
  expect_s3_class(numeric_f, "gar_Filter")
  expect_s3_class(numeric_f$numericFilter, "gar_NumericFilter")
  
  expect_s3_class(numeric_list_f, "gar_Filter")
  expect_s3_class(numeric_list_f$betweenFilter, "gar_BetweenFilter")
  
  expect_s3_class(no_nulls, "gar_Filter")
  expect_true(no_nulls$nullFilter)
  
  
})