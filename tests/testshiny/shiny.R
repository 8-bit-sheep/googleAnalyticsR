test_that("Shiny models", {
  
  loc <- ga_model_shiny_template("template_ga4")
  expect_equal(basename(loc), "template_ga4.R")
  
})