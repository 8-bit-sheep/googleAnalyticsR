.onLoad <- function(libname, pkgname) {
  
  invisible()
  ## default Google project
  options(googleAuthR.client_id = "289759286325-da3fr5kq4nl4nkhmhs2uft776kdsggbo.apps.googleusercontent.com")
  options(googleAuthR.client_secret = "1mKySbffYRyWevGkjL0LMJYu")
  options(googleAuthR.webapp.client_id = "289759286325-42j8nmkeq5n9v9eb1kiuj2i97v9oea1f.apps.googleusercontent.com")
  options(googleAuthR.webapp.client_secret = "0zBtmZ_klTEzXUaTUTP5AkNQ")
  options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics", 
                                          "https://www.googleapis.com/auth/analytics.readonly",
                                          "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                                          "https://www.googleapis.com/auth/analytics.edit",
                                          "https://www.googleapis.com/auth/analytics.manage.users"))
}

.onAttach <- function(libname, pkgname){
  
  needed <- c("https://www.googleapis.com/auth/analytics.readonly")

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

