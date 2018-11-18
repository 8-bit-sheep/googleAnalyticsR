context("Unsampled Download tests")

test_that("Scope set correctly for Google Drive tests", {
  scopes <- getOption("googleAuthR.scopes.selected")
  
  expect_true("https://www.googleapis.com/auth/drive" %in% scopes)
  
})

test_that("Can list and download unsampled files", {
  skip_on_cran()
  dl <- ga_unsampled_list(accountId = accountId,
                          webPropertyId = webPropId,
                          profileId = ga_id)
  
  expect_s3_class(dl, "data.frame")
  
  reportTitle <- dl$title

  
  dl <- ga_unsampled_download(reportTitle,
                              accountId = accountId,
                              webPropertyId = webPropId,
                              profileId = ga_id)
  
  on.exit(unlink(paste0(reportTitle,".csv")))
  # 
  expect_equal(dl, paste0(reportTitle,".csv"))
  expect_true(file.exists(paste0(reportTitle,".csv")))

  
})

# test_that("Can download one file to a data.frame", {
#   skip_on_cran()
#   reportTitle <- "googleanalyticsR_test_download"
#   
#   dl <- ga_unsampled_download(reportTitle,
#                               accountId = "54019251",
#                               webPropertyId = "UA-54019251-4",
#                               profileId = "106249469", 
#                               downloadFile = FALSE)
#   
#   expect_s3_class(dl, "data.frame")
#   
# })
# 
# 
# 
# test_that("Can download multiple files using tidyverse", {
#   skip_on_cran()
#   dl <- ga_unsampled_list(accountId = "54019251", 
#                           webPropertyId = "UA-54019251-4", 
#                           profileId = "106249469") %>%
#     dplyr::pull(title) %>%
#     purrr::map(ga_unsampled_download, 
#         accountId = "54019251", 
#         webPropertyId = "UA-54019251-4", 
#         profileId = "106249469", 
#         downloadFile = FALSE)
#   
#   expect_s3_class(dl[[1]], "data.frame")
#   
#   expect_s3_class(dl[[2]], "data.frame")
#   
# })
# 
# test_that("Can download multiple files using base R", {
#   skip_on_cran()
#   unsample_list <- ga_unsampled_list(accountId = "54019251", 
#                           webPropertyId = "UA-54019251-4", 
#                           profileId = "106249469")
#   
#   dl <- lapply(unsample_list$title, ga_unsampled_download, 
#                accountId = "54019251", 
#                webPropertyId = "UA-54019251-4", 
#                profileId = "106249469", 
#                downloadFile = FALSE)
#   
#   expect_s3_class(dl[[1]], "data.frame")
#   
#   expect_s3_class(dl[[2]], "data.frame")
#   
# })

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



