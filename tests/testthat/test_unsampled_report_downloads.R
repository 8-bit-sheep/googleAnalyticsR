# options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics",
#                                         "https://www.googleapis.com/auth/drive")) #Need drive scope, not sure if this should be default for entire package
# 
# getOptions(googleAuthR.scopes.selected)
# 
# options(googleAuthR.client_id = Sys.getenv("GA_CLIENT_ID"))
# options(googleAuthR.client_secret = Sys.getenv("GA_CLIENT_SECRET"))
# library(googleAuthR)
# gar_auth(token = ".httr-oauth")
# 
# library(dplyr)
# library(purrr)
# library(devtools)
# devtools::load_all()
# 
# reportTitle <- "googleanalyticsR_test_download"
# 
# reportList <- ga_unsampled_list(
#   accountId = Sys.getenv("ACCOUNTID"),
#   webPropertyId = Sys.getenv("WEBPROPERTYID"),
#   profileId = Sys.getenv("PROFILEID")
# )
# 
# # Download file
# ga_unsampled_download(reportList$items$title, reportTitle)
# 
# # Save to dataframe option
# df <- ga_unsampled_download(reportList, reportTitle, downloadFile = FALSE)
# 
# # Download file larger than 25 mbs
# reportTitle <- "googleanalyticsR_test_download_over_25_mb"
# ga_unsampled_download(reportList, reportTitle)
# 
# # Save to dataframe
# df <- ga_unsampled_download(reportList, reportTitle, downloadFile = FALSE)
# 
# library(tidyverse)
# 
# ## download all unsampled reports
#   reportList %>%
#   select(title) %>% #doesn't work because title is a lower level under items
#   map(ga_unsampled_download, reportList = reportList)
