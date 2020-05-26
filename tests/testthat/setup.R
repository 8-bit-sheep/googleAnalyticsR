library(testthat)
library(googleAnalyticsR)

options(googleAuthR.scopes.selected = 
          c("https://www.googleapis.com/auth/analytics",
            "https://www.googleapis.com/auth/analytics.edit",
            "https://www.googleapis.com/auth/analytics.manage.users",
            "https://www.googleapis.com/auth/analytics.user.deletion",
            "https://www.googleapis.com/auth/drive"))

# auth cache with custom project creds
googleAuthR::gar_set_client()

ga_auth(json_file = "/workspace/auth.json")

accountId <- 54019251
webPropId <- "UA-54019251-4"
ga_id <- 106249469

accountId2 <- 47480439
webPropId2 <- "UA-47480439-2"
ga_id2 <- 81416156

