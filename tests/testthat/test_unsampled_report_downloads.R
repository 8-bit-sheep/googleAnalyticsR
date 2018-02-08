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
# library(tidyverse)
# devtools::load_all()
# 
# accountId = Sys.getenv("ACCOUNTID")
# webPropertyId = Sys.getenv("WEBPROPERTYID")
# profileId = Sys.getenv("PROFILEID")
# 
# reportTitle <- "googleanalyticsR_test_download"
# 
# # Download 1 file
# ga_unsampled_download(reportTitle,
#                       accountId,
#                       webPropertyId,
#                       profileId)
# # Download multiple reports with pipes
# ga_unsampled_list(accountId, webPropertyId, profileId) %>%
#   select(title) %>%
#   unlist() %>%
#   map(ga_unsampled_download, accountId, webPropertyId, profileId)
# 
# # Download multiple reports with lapply
# ## download all unsampled reports
# unsample_df <- ga_unsampled_list(accountId, webPropertyId, profileId)
# lapply(unsample_df$title, ga_unsampled_download, accountId, webPropertyId, profileId)
# 
# # Save to one dataframe option
# df <- ga_unsampled_download(
#   reportTitle,
#   accountId,
#   webPropertyId,
#   profileId,
#   downloadFile = FALSE
# )
# 
# # Save to list of dataframes using pipes
# dfs <- ga_unsampled_list(accountId, webPropertyId, profileId) %>%
#   select(title) %>%
#   unlist() %>%
#   map(ga_unsampled_download, accountId, webPropertyId, profileId, downloadFile = FALSE)
# 
# # Save to list of dataframes with lapply
# 
# unsample_df <- ga_unsampled_list(accountId, webPropertyId, profileId)
# dfs <- lapply(unsample_df$title, ga_unsampled_download, accountId, webPropertyId, profileId, downloadFile = FALSE)
# 
# # Download 1 file larger than 25 mbs
# reportTitle <- "googleanalyticsR_test_download_over_25_mb"
# ga_unsampled_download(
#   reportTitle,
#   accountId,
#   webPropertyId,
#   profileId
# )
# 
# # Save to dataframe
# df <- ga_unsampled_download(
#   reportTitle,
#   accountId,
#   webPropertyId,
#   profileId,
#   downloadFile = FALSE
# )