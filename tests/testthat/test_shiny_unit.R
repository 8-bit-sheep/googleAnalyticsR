source("setup.R")

test_that("Shiny unit tests", {
  
  # ui tests
  add1 <- authDropdownUI("test1")
  add2 <- authDropdownUI("test1", inColumns = TRUE)
  expect_snapshot_output(add1)
  expect_snapshot_output(add2)
  
  mets1 <- multi_selectUI("test1")
  expect_snapshot_output(mets1)
  
  add3 <- accountPickerUI("test1")
  add4 <- accountPickerUI("test1", inColumns = TRUE)
  expect_snapshot_output(add3)
  expect_snapshot_output(add4)
  
  mets2 <- metricDimensionSelectUI("test1")
  expect_snapshot_output(mets2)
  
  loc <- ga_model_shiny_template("basic")
  expect_equal(basename(loc), "basic")
  
  code <- ga_model_shiny_template("basic/ui.R", read_lines = TRUE)
  expect_snapshot_output(code)
  
  m1 <- ga_model_example("decomp_ga.gamr")
  test_folder <- "basic_m1"
  ga_model_shiny(m1, local_folder = test_folder, auth_dropdown = "universal",
                 web_json = "dummy_web.json")
  expect_true(all(c("server.R","ui.R") %in% list.files(test_folder)))
  
  ui <- readLines(file.path(test_folder, "ui.R"))
  server <- readLines(file.path(test_folder, "server.R"))
  # remove variable filenames
  ui <- ui[!grepl("^model1 <-", ui)]
  server <- server[!grepl("^model1 <-", server)]  
  
  expect_snapshot_output(ui)
  expect_snapshot_output(server)
  
  unlink(test_folder, recursive = TRUE)
  
  
})