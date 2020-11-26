# offline tests
test_that("Filter DSL",{
  
  ## filter clauses
  # or string filter
  fe1 <- ga_data_filter(city=="Copenhagen" | "city" == "London")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$fieldName, "city")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$stringFilter$value, "Copenhagen")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$stringFilter$matchType, "EXACT")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$stringFilter$caseSensitive, TRUE) 
  expect_s3_class(fe1$orGroup$expressions[[1]]$filter$stringFilter, "gar_StringFilter")
  expect_s3_class(fe1$orGroup$expressions[[2]]$filter$stringFilter, "gar_StringFilter")
  expect_s3_class(fe1$orGroup, "gar_FilterExpressionList")
  
  # inlist string filter
  fe2 <- ga_data_filter(city==c("Copenhagen","London"))
  # and string filters
  fe3 <- ga_data_filter(city=="Copenhagen" & dayOfWeek == "5")
  # invert string filter
  fe4 <- ga_data_filter(!(city=="Copenhagen" | city == "London"))
  
  # multiple filter clauses
  f1 <- ga_data_filter(city==c("Copenhagen","London","Paris","New York") &
                         (dayOfWeek=="5" | dayOfWeek=="6")) 
  
  # build up complicated filters
  f2 <- ga_data_filter(f1 | "sessionSource"=="google")
  f3 <- ga_data_filter(f2 & !"sessionMedium"=="cpc")
  
  
  ## numeric filter types
  # numeric equal filter
  n1 <- ga_data_filter(sessions==5)
  # between numeric filter
  n2 <- ga_data_filter(sessions==c(5,6))
  # greater than numeric
  n3 <- ga_data_filter(sessions > 0)
  # greater than or equal
  n4 <- ga_data_filter(sessions >= 1)
  # less than numeric
  n5 <- ga_data_filter(sessions < 100)
  # less than or equal numeric
  n6 <- ga_data_filter(sessions <= 100)
  
  ## string filter types
  # begins with string
  s1 <- ga_data_filter(city %begins% "Cope")
  # ends with string
  s2 <- ga_data_filter(city %ends% "hagen")
  # contains string
  s3 <- ga_data_filter(city %in% "ope")
  # regex (full) string
  s4 <- ga_data_filter(city %regex% "^Cope")
  # regex (partial) string
  s5 <- ga_data_filter(city %regex_partial% "ope")
  
  # by default string filters are case sensitive.  
  # Use UPPERCASE operator to make then case insensitive
  
  # begins with string (case insensitive)
  su1 <- ga_data_filter(city %BEGINS% "cope")
  # ends with string (case insensitive)
  su2 <- ga_data_filter(city %ENDS% "Hagen")
  # case insensitive exact
  su3 <- ga_data_filter(city%==%"coPENGhagen")
  
})