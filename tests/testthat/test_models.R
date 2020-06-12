source("setup.R")

context("Models")

test_that("Read a model", {
  skip_on_cran()
  skip_on_travis()
  
  # load the model (equivalent to ga_model_load())
  decomp_ga <- ga_model_example("decomp_ga.gamr")
  
  expect_s3_class(decomp_ga, "ga_model")
  
  # apply model to your data
  d1 <- ga_model(ga_id, model = decomp_ga)
  
  expect_s3_class(d1, "ga_model_result")
  
  
})