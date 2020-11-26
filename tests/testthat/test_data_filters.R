# offline tests
test_that("Filter DSL",{
  
  ## filter clauses
  # or string filter
  fe1 <- ga_data_filter(city=="Copenhagen" | "city" == "London")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$fieldName, "city")
  expect_equal(fe1$orGroup$expressions[[2]]$filter$fieldName, "city")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$stringFilter$value, "Copenhagen")
  expect_equal(fe1$orGroup$expressions[[2]]$filter$stringFilter$value, "London")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$stringFilter$matchType, "EXACT")
  expect_equal(fe1$orGroup$expressions[[1]]$filter$stringFilter$caseSensitive, TRUE) 
  expect_s3_class(fe1$orGroup$expressions[[1]]$filter$stringFilter, "gar_StringFilter")
  expect_s3_class(fe1$orGroup$expressions[[2]]$filter$stringFilter, "gar_StringFilter")
  expect_s3_class(fe1$orGroup, "gar_FilterExpressionList")
  expect_s3_class(fe1, "gar_FilterExpression")
  
  # inlist string filter
  fe2 <- ga_data_filter(city==c("Copenhagen","London"))
  expect_s3_class(fe2$inListFilter, "gar_InListFilter")
  expect_s3_class(fe2, "gar_Filter")
  # and string filters
  fe3 <- ga_data_filter(city=="Copenhagen" & dayOfWeek == "5")
  expect_s3_class(fe3, "gar_FilterExpression")
  expect_s3_class(fe3$andGroup, "gar_FilterExpressionList")
  expect_s3_class(fe3$andGroup$expressions[[1]], "gar_FilterExpression")
  expect_s3_class(fe3$andGroup$expressions[[2]], "gar_FilterExpression")
  expect_s3_class(fe3$andGroup$expressions[[2]]$filter, "gar_Filter")
  expect_s3_class(fe3$andGroup$expressions[[1]]$filter, "gar_Filter")
  # invert string filter
  fe4 <- ga_data_filter(!(city=="Copenhagen" | city == "London"))
  expect_s3_class(fe4, "gar_FilterExpression")  
  expect_s3_class(fe4$notExpression, "gar_FilterExpression")
  expect_s3_class(fe4$notExpression$orGroup, "gar_FilterExpressionList")
  # multiple filter clauses
  f1 <- ga_data_filter(city==c("Copenhagen","London","Paris","New York") &
                         (dayOfWeek=="5" | dayOfWeek=="6")) 
  
  # build up complicated filters
  f2 <- ga_data_filter(f1 | "sessionSource"=="google")
  f3 <- ga_data_filter(f2 & !"sessionMedium"=="cpc")
  expect_s3_class(f3, "gar_FilterExpression")  
  expect_s3_class(f3$andGroup, "gar_FilterExpressionList")
  expect_s3_class(
    f3$andGroup$expressions[[1]]$orGroup, "gar_FilterExpressionList")
  expect_s3_class(
    f3$andGroup$expressions[[1]]$orGroup$expressions[[1]]$andGroup$expressions[[1]]$filter,
                  "gar_Filter")
  expect_s3_class(
    f3$andGroup$expressions[[1]]$orGroup$expressions[[1]]$andGroup$expressions[[1]]$filter$inListFilter,
                  "gar_InListFilter")
  
  ## numeric filter types
  # numeric equal filter
  n1 <- ga_data_filter(sessions==5)
  expect_s3_class(n1, "gar_Filter")
  expect_s3_class(n1$numericFilter, "gar_NumericFilter")
  # between numeric filter
  n2 <- ga_data_filter(sessions==c(5,6))
  expect_s3_class(n2, "gar_Filter")
  expect_s3_class(n2$betweenFilter, "gar_BetweenFilter")
  # greater than numeric
  n3 <- ga_data_filter(sessions > 0)
  expect_s3_class(n3, "gar_Filter")
  expect_s3_class(n3$numericFilter, "gar_NumericFilter")
  expect_equal(n3$numericFilter$operation, "GREATER_THAN")
  # greater than or equal
  n4 <- ga_data_filter(sessions >= 1)
  expect_s3_class(n4, "gar_Filter")
  expect_s3_class(n4$numericFilter, "gar_NumericFilter")
  expect_equal(n4$numericFilter$operation, "GREATER_THAN_OR_EQUAL")
  # less than numeric
  n5 <- ga_data_filter(sessions < 100)
  expect_s3_class(n5, "gar_Filter")
  expect_s3_class(n5$numericFilter, "gar_NumericFilter")
  expect_equal(n5$numericFilter$operation, "LESS_THAN")
  # less than or equal numeric
  n6 <- ga_data_filter(sessions <= 100)
  expect_s3_class(n6, "gar_Filter")
  expect_s3_class(n6$numericFilter, "gar_NumericFilter")
  expect_equal(n6$numericFilter$operation, "LESS_THAN_OR_EQUAL")
  
  ## string filter types
  # begins with string
  s1 <- ga_data_filter(city %begins% "Cope")
  expect_s3_class(s1, "gar_Filter")
  expect_s3_class(s1$stringFilter, "gar_StringFilter")
  expect_equal(s1$stringFilter$matchType, "BEGINS_WITH")
  expect_equal(s1$stringFilter$caseSensitive, TRUE)
  # ends with string
  s2 <- ga_data_filter(city %ends% "hagen")
  expect_s3_class(s2, "gar_Filter")
  expect_s3_class(s2$stringFilter, "gar_StringFilter")
  expect_equal(s2$stringFilter$matchType, "ENDS_WITH")
  expect_equal(s2$stringFilter$caseSensitive, TRUE)
  # contains string
  s3 <- ga_data_filter(city %contains% "ope")
  expect_s3_class(s3, "gar_Filter")
  expect_s3_class(s3$stringFilter, "gar_StringFilter")
  expect_equal(s3$stringFilter$matchType, "CONTAINS")
  expect_equal(s3$stringFilter$caseSensitive, TRUE)
  # regex (full) string
  s4 <- ga_data_filter(city %regex% "^Cope")
  expect_s3_class(s4, "gar_Filter")
  expect_s3_class(s4$stringFilter, "gar_StringFilter")
  expect_equal(s4$stringFilter$matchType, "FULL_REGEXP")
  expect_equal(s4$stringFilter$caseSensitive, TRUE)
  # regex (partial) string
  s5 <- ga_data_filter(city %regex_partial% "ope")
  expect_s3_class(s5, "gar_Filter")
  expect_s3_class(s5$stringFilter, "gar_StringFilter")
  expect_equal(s5$stringFilter$matchType, "PARTIAL_REGEXP")
  expect_equal(s5$stringFilter$caseSensitive, TRUE)
  
  # by default string filters are case sensitive.  
  # Use UPPERCASE operator to make then case insensitive
  
  # begins with string (case insensitive)
  su1 <- ga_data_filter(city %BEGINS% "cope")
  expect_s3_class(su1, "gar_Filter")
  expect_s3_class(su1$stringFilter, "gar_StringFilter")
  expect_equal(su1$stringFilter$matchType, "BEGINS_WITH")
  expect_equal(su1$stringFilter$caseSensitive, FALSE)
  # ends with string (case insensitive)
  su2 <- ga_data_filter(city %ENDS% "Hagen")
  expect_s3_class(su2, "gar_Filter")
  expect_s3_class(su2$stringFilter, "gar_StringFilter")
  expect_equal(su2$stringFilter$matchType, "ENDS_WITH")
  expect_equal(su2$stringFilter$caseSensitive, FALSE)
  # case insensitive exact
  su3 <- ga_data_filter(city%==%"coPENGhagen")
  expect_s3_class(su3, "gar_Filter")
  expect_s3_class(su3$stringFilter, "gar_StringFilter")
  expect_equal(su3$stringFilter$matchType, "EXACT")
  expect_equal(su3$stringFilter$caseSensitive, FALSE)
  
})