library(testthat)
library(googleAnalyticsR)

options(googleAuthR.scopes.selected = 
          c("https://www.googleapis.com/auth/analytics",
            "https://www.googleapis.com/auth/analytics.edit",
            "https://www.googleapis.com/auth/analytics.manage.users",
            "https://www.googleapis.com/auth/analytics.user.deletion",
            "https://www.googleapis.com/auth/drive"),
        googleAuthR.httr_oauth_cache = Sys.getenv("GA_AUTH_FILE"),
        googleAuthR.client_id = "289759286325-da3fr5kq4nl4nkhmhs2uft776kdsggbo.apps.googleusercontent.com",
        googleAuthR.client_secret = "1mKySbffYRyWevGkjL0LMJYu")

# auth cache with default project creds
ga_auth(email = Sys.getenv("GARGLE_EMAIL"))

# auth cache with custom project creds
googleAuthR::gar_set_client()
ga_auth(email = Sys.getenv("GARGLE_EMAIL"))

accountId <- 54019251
webPropId <- "UA-54019251-4"
ga_id <- 106249469

accountId2 <- 47480439
webPropId2 <- "UA-47480439-2"
ga_id2 <- 81416156

## auto auth
local_auth <- "GA_AUTH_FILE"


