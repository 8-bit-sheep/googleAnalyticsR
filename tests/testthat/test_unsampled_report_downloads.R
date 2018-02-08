context("Unsampled Download tests")

test_that("Can list unsampled files", {
  
  dl <- ga_unsampled_list(accountId = "54019251",
                          webPropertyId = "UA-54019251-4",
                          profileId = "129374148")
  
  expect_s3_class(dl, "data.frame")
  
})

test_that("Can download one file", {
  
  reportTitle <- "googleanalyticsR_test_download"

  
  dl <- ga_unsampled_download(reportTitle,
                              accountId = "54019251",
                              webPropertyId = "UA-54019251-4",
                              profileId = "129374148")
  on.exit(unlink(paste0(reportTitle,".csv")))
  
  expect_equal(dl, paste0(reportTitle,".csv"))
  expect_true(file.exists(paste0(reportTitle,".csv")))

  
})

test_that("Can download one file to a data.frame", {
  
  reportTitle <- "googleanalyticsR_test_download"
  
  dl <- ga_unsampled_download(reportTitle,
                              accountId = "54019251",
                              webPropertyId = "UA-54019251-4",
                              profileId = "129374148", 
                              downloadFile = FALSE)
  
  expect_s3_class(dl, "data.frame")
  
})



test_that("Can download multiple files using tidyverse", {
  
  dl <- ga_unsampled_list(accountId = "54019251", 
                          webPropertyId = "UA-54019251-4", 
                          profileId = "129374148") %>%
    dplyr::pull(title) %>%
    purrr::map(ga_unsampled_download, 
        accountId = "54019251", 
        webPropertyId = "UA-54019251-4", 
        profileId = "129374148", 
        downloadFile = FALSE)
  
  expect_s3_class(dl[[1]], "data.frame")
  
  expect_s3_class(dl[[2]], "data.frame")
  
})


# I don't have a >25MB export to test yet 
# test_that("Can download file over 25MB", {
#   
#   reportTitle <- "googleanalyticsR_test_download_over_25_mb"
#   
#   dl <- ga_unsampled_download(
#     reportTitle,
#     accountId = "54019251", 
#     webPropertyId = "UA-54019251-4", 
#     profileId = "129374148"
#   )
#   
#   expect_true(file.exists(paste0(reportTitle,".csv")))
#   
# })



