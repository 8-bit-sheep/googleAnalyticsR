context("App + Web Tests")

test_that("Basic fetch", {
  skip_on_cran()
  ga_auth()
  df <- ga_data(
    206670707,
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
})

context("App + Web Filters")

test_that("Filter objects", {

  string_f <- ga_aw_filter("city","Copenhagen","EXACT", caseSensitive = FALSE)
  in_list_f <- ga_aw_filter("city",c("Copenhagen","London"))
  numeric_f <- ga_aw_filter("activeUsers", 1L, "GREATER_THAN")
  # need to create metric expressions that are floats
  #float_f <- ga_aw_filter("")
  numeric_list_f <- ga_aw_filter("activeUsers", c(1L,3L))
  no_nulls <- ga_aw_filter("city", NULL)
  
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

test_that("Filter fetch types", {
  skip_on_cran()
  test_filter <- function(dim_filter = NULL,
                          met_filter = NULL){
    ga_data(
      206670707,
      metrics = "activeUsers",
      dimensions = c("date","city", "dayOfWeek"),
      date_range = c("2020-03-31", "2020-04-27"),
      dimensionFilter = dim_filter,
      metricFilter = met_filter,
      limit = 100
    )
  }
  
  #dimension filter
  string_f <- ga_aw_filter("city","Copenhagen","EXACT", caseSensitive = FALSE)
  string_data <- test_filter(string_f)
  expect_equal(unique(string_data$city), "Copenhagen")
  
  in_list_f <- ga_aw_filter("city",c("Copenhagen","London"))
  in_list_data <- test_filter(in_list_f)
  expect_true(all(unique(in_list_data$city) %in% c("London", "Copenhagen")))
  
  # metric filters
  numeric_f <- ga_aw_filter("activeUsers", 2L, "GREATER_THAN")
  numeric_data <- test_filter(met_filter = numeric_f)
  expect_true(all(numeric_data$activeUsers >2))
  
  numeric_list_f <- ga_aw_filter("activeUsers", c(2L,6L))
  numeric_list_data <- test_filter(met_filter = numeric_list_f)
  expect_true(all(numeric_list_data$activeUsers %in% 2:6))
  
  # need to create metric expressions that are floats
  #float_f <- ga_aw_filter("")
  
  # what can I test this on?
  #no_nulls <- ga_aw_filter("city", TRUE)
  
})

