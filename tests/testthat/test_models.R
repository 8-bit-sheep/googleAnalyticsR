source("setup.R")

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

  model2 <- ga_model_edit(model, description = "changed",
                          outputShiny = shiny::plotOutput, renderShiny = shiny::renderPlot)
  expect_equal(model2$description, "changed")

  expect_snapshot(model2$shiny_module$ui("test"))

  ga_model_write(model, "test_model_write.R")
  expect_true(file.exists("test_model_write.R"))

  unlink("test_model_write.R")
  unlink("test.gamr")

})

test_that("Model Shiny unit tests",{
  
  inputShiny1 <- shiny::dateInput("test_id1","Date")
  inputShiny2 <- shiny::checkboxInput("test_id2", "Checkbox")
  inputShiny3 <- shiny::selectInput("test_id3","Select",choices = c(1,2,3))
  
  outputShiny1 <- shiny::plotOutput
  outputShiny2 <- shiny::textOutput
  outputShiny3 <- shiny::uiOutput
  
  input_id <- extract_from_list(inputShiny1, id_regex = "id$")
  test1 <- create_shiny_module_ui(outputShiny1, inputShiny1, input_id)
  t1 <- test1("my_ns")
  expect_equal(t1[[1]]$attribs$id, "my_ns-test_id1")
  expect_equal(t1[[2]]$attribs$id, "my_ns-ui_out")

  input_id <- extract_from_list(inputShiny2, id_regex = "id$")
  test2 <- create_shiny_module_ui(outputShiny2, inputShiny2, input_id)
  
  t2 <- test2("my_ns")
  expect_equal(t2[[1]]$children[[1]]$children[[1]]$children[[1]]$attribs$id, 
               "my_ns-test_id2")
  expect_equal(t2[[2]]$attribs$id, "my_ns-ui_out")
  
  input_id <- extract_from_list(inputShiny3, id_regex = "id$")
  test3 <- create_shiny_module_ui(outputShiny3, inputShiny3, input_id)
  
  t3 <- test3("my_ns")
  expect_equal(t3[[1]]$children[[1]]$attribs[["for"]], "my_ns-test_id3")
  expect_equal(t3[[1]]$children[[2]]$children[[1]]$attribs$id, "my_ns-test_id3")  
  expect_equal(t3[[2]]$attribs$id, "my_ns-ui_out")
  
  # multiple model inputs for one model output
  multiModelsInput <- shiny::tagList(inputShiny1, inputShiny2, inputShiny3)
  input_ids <- extract_ids(multiModelsInput)
  ui <- create_shiny_module_ui(outputShiny1, multiModelsInput, input_ids)
  
  t4 <- ui("my_ns")
  expect_equal(t1[[2]]$attribs$id, t4[[2]]$attribs$id)
  expect_equal(t4[[2]]$attribs$id, "my_ns-ui_out")
  expect_equal(t4[[1]][[1]], t1[[1]])
  expect_equal(t4[[1]][[2]], t2[[1]])
  expect_equal(t4[[1]][[3]], t3[[1]])
  
  is <- shiny::tagList(
        shiny::numericInput("first_day", "First day minimum pageviews",
                            value = 2, min=0, max=100),
        shiny::numericInput("total_min_cutoff", "Minimum Total pageviews",
                            value = 500, min = 0, max = 1000),
        shiny::numericInput("days_live", label = "Days Live",
                            value = 60, min = 10, max = 400),
        shiny::textInput("page_regex", label = "Page filter regex", 
                         value = ".*")
  )
  
  input_ids <- extract_ids(is)
  expect_equal(input_ids[[1]], "first_day")
  expect_equal(input_ids[[2]], "total_min_cutoff")
  expect_equal(input_ids[[3]], "days_live")
  expect_equal(input_ids[[4]], "page_regex")
  mod_ui <- create_shiny_module_ui(shiny::plotOutput, is, input_ids)
  new_ui <- mod_ui("my_ns")
  new_ids <- extract_ids(new_ui[[1]])[[1]]
  expect_equal(new_ids[[1]], "my_ns-first_day")
  expect_equal(new_ids[[2]], "my_ns-total_min_cutoff")
  expect_equal(new_ids[[3]], "my_ns-days_live")
  expect_equal(new_ids[[4]], "my_ns-page_regex")
  
  
})

