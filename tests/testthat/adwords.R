# source("setup.R")
# 
# context("Adwords")
# 
# test_that("Get AdWords Link meta data", {
#   skip_on_cran()
#   
#   adwords_list <- ga_adwords_list(accountId, webPropId)
#   
#   expect_s3_class(adwords_list, "data.frame")
#   
# })
# 
# test_that("Get information about specific GA Adwords link", {
#   skip_on_cran()
#   adwords_list <- ga_adwords_list(accountId, webPropId)
#   #assumes that default account from setup.R has existing link to Adwords account
#   ga_adwords_link <- ga_adwords(accountId, webPropId, webPropertyAdWordsLinkId = adwords_list[1,1])
#   
#   expect_equal(ga_adwords_link$kind, "analytics#entityAdWordsLink")
#   
# })