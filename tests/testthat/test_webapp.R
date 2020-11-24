context("App + Web Tests")

test_that("Basic fetch", {
  
  df <- ga_data(
    206670707,
    metrics = "activeUsers",
    dimensions = c("date","city"),
    date_range = c("2020-03-31", "2020-04-27"),
    limit = 100
  )
  
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("date","city","activeUsers"))
  expect_s3_class(df$activeUsers, "numeric")
  expect_s3_class(df$date, "Date")
  expect_s3_class(df$city, "character")
})

context("App + Web Filters")

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

test_that("Filter fetch types", {

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
  expect_equal(unique(in_list_data$city), c("London", "Copenhagen"))
  
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

test_that("Filter DSL",{
  
  ## filter clauses
  # or string filter
  ga_data_filter("city"=="Copenhagen" | "city" == "London")
  # inlist string filter
  ga_data_filter("city"==c("Copenhagen","London"))
  # and string filters
  ga_data_filter("city"=="Copenhagen" & "dayOfWeek" == "5")
  # invert string filter
  ga_data_filter(!("city"=="Copenhagen" | "city" == "London"))
  
  # multiple filter clauses
  f1 <- ga_data_filter("city"==c("Copenhagen","London","Paris","New York") &
                         ("dayOfWeek"=="5" | "dayOfWeek"=="6")) 
  f1
  
  # build up complicated filters
  f2 <- ga_data_filter(f1 | "sessionSource"=="google")
  f2
  f3 <- ga_data_filter(f2 & !"sessionMedium"=="cpc")
  f3
  
  ## numeric filter types
  # numeric equal filter
  ga_data_filter("sessions"==5)
  # between numeric filter
  ga_data_filter("sessions"==c(5,6))
  # greater than numeric
  ga_data_filter("sessions" > 0)
  # greater than or equal
  ga_data_filter("sessions" >= 1)
  # less than numeric
  ga_data_filter("sessions" < 100)
  # less than or equal numeric
  ga_data_filter("sessions" <= 100)
  
  ## string filter types
  # begins with string
  ga_data_filter("city" %begins% "Cope")
  # ends with string
  ga_data_filter("city" %ends% "hagen")
  # contains string
  ga_data_filter("city" %in% "ope")
  # regex (full) string
  ga_data_filter("city" %regex% "^Cope")
  # regex (partial) string
  ga_data_filter("city" %regex_partial% "ope")
  
  # by default string filters are case sensitive.  
  # Use UPPERCASE operator to make then case insensitive
  
  # begins with string (case insensitive)
  ga_data_filter("city" %BEGINS% "cope")
  # ends with string (case insensitive)
  ga_data_filter("city" %ENDS% "Hagen")
  # case insensitive exact
  ga_data_filter("city"%==%"coPENGhagen")
  
})