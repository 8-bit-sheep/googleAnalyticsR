context("ganalytics integration")

test_that("ganalytics filters", {
  library(ganalytics)
  # Device category is desktop or tablet - a dimension filter using an OR condition.
  desktop_or_mobile <- Expr(~deviceCategory == "desktop") | Expr(~deviceCategory == "tablet")
  
  # New visitors using either a desktop or tablet device - a dimension filter involving both an AND and an OR condition.
  new_desktop_and_mobile_visitors <- Expr(~userType == "new") & desktop_or_mobile
  
  # At least one goal completion or transaction - a metric filter using an OR condition.
  at_least_one_conversion <- Expr(~goalCompletionsAll > 0) | Expr(~transactions > 0)
  
  results <- google_analytics(
    viewId = ga_id2,
    date_range = c("30daysAgo", "yesterday"),
    metrics = c("users", "sessions", "goalCompletionsAll", "transactions"),
    dimensions = c("deviceCategory", "userType"),
    dim_filters = new_desktop_and_mobile_visitors,
    met_filters = at_least_one_conversion
  )
  
  expect_s3_class(results, "data.frame")
  
})

test_that("ganalytics segments", {
  library(ganalytics)
  bounces <- Expr(~bounces != 0)
  mobile_or_tablet <- Expr(~deviceCategory %in% c("mobile", "tablet"))
  multi_session_users <- Include(PerUser(Expr(~sessions > 1)), scope = "users")
  
  my_segment_list <- list(
    bounced_sessions = PerSession(bounces),
    mobile_or_tablet = mobile_or_tablet,
    multi_session_users = multi_session_users
  )
  
  results <- google_analytics(
    viewId = ga_id,
    date_range = c("30daysAgo", "yesterday"),
    metrics = c("users", "sessions"),
    dimensions = c("segment"),
    segments = Segments(my_segment_list)
  )
  
  expect_s3_class(results, "data.frame")
  
})