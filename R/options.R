.onLoad <- function(libname, pkgname) {
  
  invisible()
  
}

.onAttach <- function(libname, pkgname){
  
  needed <- c("https://www.googleapis.com/auth/analytics", 
              "https://www.googleapis.com/auth/analytics.readonly",
              "https://www.googleapis.com/auth/analytics.manage.users.readonly",
              "https://www.googleapis.com/auth/analytics.edit",
              "https://www.googleapis.com/auth/analytics.manage.users",
              "https://www.googleapis.com/auth/analytics.provision"	)

  googleAuthR::gar_attach_auto_auth(needed, "GA_AUTH_FILE", "TRAVIS_GA_AUTH_FILE")
  
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
  
  invisible()
  
}

