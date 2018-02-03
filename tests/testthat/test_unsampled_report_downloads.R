options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics",
                                        "https://www.googleapis.com/auth/drive")) #Need drive scope, not sure if this should be default for entire package

getOptions(googleAuthR.scopes.selected)

options(googleAuthR.client_id = Sys.getenv("GA_CLIENT_ID"))
options(googleAuthR.client_secret = Sys.getenv("GA_CLIENT_SECRET"))
library(googleAuthR)
gar_auth(token = ".httr-oauth")

library(dplyr)
library(purrr)
library(devtools)
devtools::load_all()

accountId = Sys.getenv("ACCOUNTID")
webPropertyId = Sys.getenv("WEBPROPERTYID")
profileId = Sys.getenv("PROFILEID")

library(tidyverse)
## download all unsampled reports and create a list of dataframes
test <- ga_unsampled_list(accountId, webPropertyId, profileId) %>%
  select(driveDownloadDetails, title) %>% 
  na.omit() 

#download
walk2(unlist(test$driveDownloadDetails), 
      unlist(test$title),
      ga_unsampled_download)

#list of dataframes
dataframes_test <- map(unlist(test$driveDownloadDetails), 
      ga_unsampled_download, downloadFile=FALSE)
  
## one small report  < 25 mb

reportTitle <- "googleanalyticsR_test_download"

#download - not working right now due to misuse of walk2 probably
ga_unsampled_list(accountId, webPropertyId, profileId) %>%
  filter(title == reportTitle) %>%
  select(driveDownloadDetails, title) %>% 
  na.omit() %>%
  walk2(ga_unsampled_download)

#works
small <- ga_unsampled_list(accountId, webPropertyId, profileId) %>%
  filter(title == reportTitle) %>%
  select(driveDownloadDetails) %>% 
  na.omit() %>%
  walk(ga_unsampled_download)

#doesn't work, but should in case user selects title too
small <- ga_unsampled_list(accountId, webPropertyId, profileId) %>%
  filter(title == reportTitle) %>%
  select(driveDownloadDetails, title) %>% 
  na.omit() %>%
  map2(ga_unsampled_download)

## one large report > 25 mb
large_reportTitle <- "googleanalyticsR_test_download_over_25_mb"

#download - not working right now due to misuse of walk2 probably
ga_unsampled_list(accountId, webPropertyId, profileId) %>%
  filter(title == large_reportTitle) %>%
  select(driveDownloadDetails, title) %>% 
  na.omit() %>%
  walk2(ga_unsampled_download)

large <- ga_unsampled_list(accountId, webPropertyId, profileId) %>%
  filter(title == large_reportTitle) %>%
  select(driveDownloadDetails, title) %>% 
  na.omit() %>%
  map2(ga_unsampled_download)
