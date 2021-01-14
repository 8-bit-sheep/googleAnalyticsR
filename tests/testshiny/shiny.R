test_that("Shiny models", {
  
  loc <- ga_model_shiny_template("template_ga4")
  expect_equal(basename(loc), "template_ga4.R")
  
  m1 <- ga_model_example("decomp_ga.gamr")
  m2 <- ga_model_example("decomp_ga_advanced.gamr")
  m3 <- ga_model_example("time-normalised.gamr")
  m4 <- ga_model_example("ga-effect.gamr")
  
  # launch single shiny app
  ga_model_shiny(m1, template = ga_model_shiny_template("template_ua.R"))
  ga_model_shiny(m2, template = ga_model_shiny_template("template_ua.R"))
  
  # launch multiple models in one shiny app
  ga_model_shiny(list(m4,m2, m3), 
                 template = ga_model_shiny_template("multiple_ua.R"))
  
  ga_model_shiny(list(m4,m3), 
                 template = ga_model_shiny_template("gentelella.R"))
  
})
