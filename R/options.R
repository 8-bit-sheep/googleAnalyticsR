.onLoad <- function(libname, pkgname) {
  
  invisible()
  
}

.onAttach <- function(libname, pkgname){
  
  scopes <- getOption("googleAuthR.scopes.selected")
  needed <- c("https://www.googleapis.com/auth/analytics", 
              "https://www.googleapis.com/auth/analytics.readonly",
              "https://www.googleapis.com/auth/analytics.manage.users.readonly",
              "https://www.googleapis.com/auth/analytics.edit",
              "https://www.googleapis.com/auth/analytics.manage.users",
              "https://www.googleapis.com/auth/analytics.provision"	)

  
  if(!any(needed %in% scopes)){
    ## no analytics scopes
    packageStartupMessage("No analytics scopes found so setting scope to https://www.googleapis.com/auth/analytics.readonly")
    packageStartupMessage("If you need other scopes set do so via options(googleAuthR.scopes.selected = c('scope1', 'scope2')) before loading library(googleAnalyticsR) and include one analytics scope.")
    new_scopes <- "https://www.googleapis.com/auth/analytics.readonly"
  } else {
    ## analytics scopes set
    new_scopes <- scopes
  }
  
  options(googleAuthR.scopes.selected = new_scopes)
  
  if(Sys.getenv("GA_CLIENT_ID") != ""){
    options(googleAuthR.client_id = Sys.getenv("GA_CLIENT_ID"))
  }
  
  if(Sys.getenv("GA_CLIENT_SECRET") != ""){
    options(googleAuthR.client_secret = Sys.getenv("GA_CLIENT_SECRET"))
  }
  
  if(Sys.getenv("GA_WEB_CLIENT_ID") != ""){
    options(googleAuthR.webapp.client_id = Sys.getenv("GA_WEB_CLIENT_ID"))
  }
  
  if(Sys.getenv("GA_WEB_CLIENT_SECRET") != ""){
    options(googleAuthR.webapp.client_id = Sys.getenv("GA_WEB_CLIENT_SECRET"))
  }
  
  
  if(Sys.getenv("GA_AUTH_FILE") != ""){
    googleAnalyticsR::ga_auth()
    packageStartupMessage("Successfully authenticated via ", Sys.getenv("GA_AUTH_FILE"))
  }
  
  invisible()
  
}

