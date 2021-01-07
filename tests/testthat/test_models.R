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
  expect_true(file.exists("Rplots.pdf"))
  on.exit(unlink("Rplots.pdf"))


})

test_that("Make a model",{
  skip_on_cran()
  skip_on_travis()
  
  # fetch data
  data_f <- function(view_id,
                     date_range = c(Sys.Date() - 300, Sys.Date()),
                     ...) {
    google_analytics(view_id,
                     date_range = date_range,
                     metrics = "sessions",
                     dimensions = "date",
                     max = -1)
  }

  # model data
  model_f <- function(df, ...) {
    decompose(ts(df$sessions, frequency = 7))
  }

  # output data
  output_f <- function(df, ...) plot(df)

  tm <- ga_model_make(
    data_f = data_f,
    required_columns = "date",
    model_f = model_f,
    output_f = output_f,
    description = "A description"
  )

  expect_s3_class(tm, "ga_model")

  ga_model_save(tm, filename = "test.gamr")
  expect_true(file.exists("test.gamr"))


})

test_that("Run a model online",{
  skip_on_cran()
  skip_on_travis()

  model <- ga_model_load("test.gamr")
  expect_s3_class(model, "ga_model")

  md <- model$data_f(ga_id)
  expect_true(is.data.frame(md))

  mm <- model$model_f(md)
  expect_s3_class(mm, "decomposed.ts")

  expect_equal(model$description, "A description")

  model2 <- ga_model_edit(model, description = "changed")
  expect_equal(model2$description, "changed")

  expect_equal(as.character(model$shiny_module$ui("test")),
               "<div id=\"test-ui_out\" class=\"shiny-plot-output\" style=\"width: 100% ; height: 400px\"></div>")

  ga_model_write(model, "test_model_write.R")
  expect_true(file.exists("test_model_write.R"))

  on.exit(unlink("test_model_write.R"))
  on.exit(unlink("test.gamr"))

})

