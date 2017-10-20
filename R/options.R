.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googleAnalyticsR <- list(
    ## default Google project
    googleAuthR.client_id = "289759286325-da3fr5kq4nl4nkhmhs2uft776kdsggbo.apps.googleusercontent.com",
    googleAuthR.client_secret = "1mKySbffYRyWevGkjL0LMJYu",
    googleAuthR.webapp.client_id = "289759286325-42j8nmkeq5n9v9eb1kiuj2i97v9oea1f.apps.googleusercontent.com",
    googleAuthR.webapp.client_secret = "0zBtmZ_klTEzXUaTUTP5AkNQ",
    googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/analytics", 
                                    "https://www.googleapis.com/auth/analytics.readonly",
                                    "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                                    "https://www.googleapis.com/auth/analytics.edit",
                                    "https://www.googleapis.com/auth/analytics.manage.users")
  )
  
  toset <- !(names(op.googleAnalyticsR) %in% names(op))
  ## only set those not set already
  if(any(toset)) options(op.googleAnalyticsR[toset])
  
  ## override existing options
  options(
    googleAuthR.batch_endpoint = "https://www.googleapis.com/batch/analytics/v3")
  
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
  
  default_project_message()
  
  f <- function(req){
    if(!is.null(req$content$reports)){
      return(TRUE)
    } else {
      return(FALSE)
    }}

  googleAuthR::gar_cache_setup(invalid_func = f)
  
  invisible()
  
}

.onAttach <- function(libname, pkgname){
  
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
  
  needed <- c("https://www.googleapis.com/auth/analytics.readonly")
  
  googleAuthR::gar_attach_auto_auth(needed, 
                                    environment_var = "GA_AUTH_FILE")
  
  invisible()
  
}

