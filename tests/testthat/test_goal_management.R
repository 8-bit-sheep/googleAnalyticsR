source("setup.R")

context("Goal management")

test_that("Get Goal list for view", {
  skip_on_cran()
  
  goals <- ga_goal_list(accountId, webPropId, ga_id)
  
  expect_equal(goals$kind, "analytics#goals")
  
})

test_that("Get Specific Goal", {
  skip_on_cran()
  
  goal <- ga_goal(accountId, webPropId, ga_id , goalId = 1)
  
  expect_equal(goal$kind, "analytics#goal")
  
})

test_that("Add goal to the view", {
  skip_on_cran()
  
  Goal <- list(
    id = '20',
    active = TRUE,
    name = 'Visited more than 3 pages',
    type = 'VISIT_NUM_PAGES',
    visitNumPagesDetails = list(
      comparisonType = 'GREATER_THAN',
      comparisonValue = 3
    )
  )
  
  # will fail if exists
  expect_error( ga_goal_add(Goal, 
                            accountId = accountId2, 
                            webPropertyId = webPropId2, 
                            viewId = ga_id2), "Entity with id 20 already exists")

  
  Goal1 <- list(
    active = TRUE,
    name = 'Visited more than 4 pages',
    type = 'VISIT_NUM_PAGES',
    visitNumPagesDetails = list(
      comparisonType = 'GREATER_THAN',
      comparisonValue = 4
    )
  )
  
  # method not defined, PUT applied by default
  response2 <- ga_goal_update(Goal1, accountId2, webPropId2, ga_id2, 20) 
  
  expect_equal(response2$kind, "analytics#goal")
  expect_equal(response2$name, Goal1$name)
  
  # only the field we're changing needed because we now use the PATCH method
  Goal2 <- list(
    name = 'Visited over 4 pages'
  )
  
  response3 <- ga_goal_update(Goal2, accountId2, webPropId2, ga_id2, 20, method = "PATCH") 
  
  expect_equal(response3$kind, "analytics#goal")
  expect_equal(response3$name, Goal2$name)
  
})

