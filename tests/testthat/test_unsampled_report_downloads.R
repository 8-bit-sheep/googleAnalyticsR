options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics",
                                        "https://www.googleapis.com/auth/drive")) #Need drive scope, not sure if this should be default for entire package
options(googleAuthR.client_id = Sys.getenv("GA_CLIENT_ID"))
options(googleAuthR.client_secret = Sys.getenv("GA_CLIENT_SECRET"))
library(googleAuthR)
gar_auth(token = ".httr-oauth")

library(dplyr)
library(purrr)
library(devtools)
devtools::load_all()

reportTitle <- "googleanalyticsR_test_download"

# Download file
ga_unsampled_download(accountId = Sys.getenv("ACCOUNTID"),
                      webPropertyId = Sys.getenv("WEBPROPERTYID"),
                      profileId = Sys.getenv("PROFILEID"),
                      reportTitle)

# Save to dataframe option
df <- ga_unsampled_download(accountId = Sys.getenv("ACCOUNTID"),
                      webPropertyId = Sys.getenv("WEBPROPERTYID"),
                      profileId = Sys.getenv("PROFILEID"),
                      reportTitle,
                      downloadFile = FALSE)

# Download file larger than 25 mbs
reportTitle <- "googleanalyticsR_test_download_over_25_mb"
ga_unsampled_download(accountId = Sys.getenv("ACCOUNTID"),
                      webPropertyId = Sys.getenv("WEBPROPERTYID"),
                      profileId = Sys.getenv("PROFILEID"),
                      reportTitle)

# Save to dataframe
df <- ga_unsampled_download(accountId = Sys.getenv("ACCOUNTID"),
                            webPropertyId = Sys.getenv("WEBPROPERTYID"),
                            profileId = Sys.getenv("PROFILEID"),
                            reportTitle,
                            downloadFile = FALSE)