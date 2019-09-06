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
                                    "https://www.googleapis.com/auth/analytics.edit",
                                    "https://www.googleapis.com/auth/analytics.manage.users",
                                    "https://www.googleapis.com/auth/analytics.user.deletion"),
    googleAuthR.httr_oauth_cache = "ga.oauth",
    googleAuthR.quotaUser = Sys.info()[["user"]]
  )
  
  toset <- !(names(op.googleAnalyticsR) %in% names(op))
  ## only set those not set already
  if(any(toset)) options(op.googleAnalyticsR[toset])
  
  ## override existing options
  options(
    googleAuthR.batch_endpoint = "https://www.googleapis.com/batch/analytics/v3",
    googleAuthR.tryAttempts = 1)
  
  f <- function(req){
    
    stuff <- tryCatch(req$content$reports, error = function(x) NULL)
    
    # data is not golden
    if(!is.null(stuff[[1]]$data$isDataGolden) && !stuff[[1]]$data$isDataGolden){
      return(FALSE)
    }
    
    # present only if including today's data? e.g. not golden?
    if(!is.null(stuff[[1]]$data$dataLastRefreshed)){
      return(FALSE)
    }

    if(!is.null(stuff)){
      return(TRUE)
    } else {
      return(FALSE)
    }}

  googleAuthR::gar_cache_setup(invalid_func = f)
  
  invisible()
  
}

.onAttach <- function(libname, pkgname){
  
  if(Sys.getenv("GAR_CLIENT_JSON") != ""){
    googleAuthR::gar_set_client(json = Sys.getenv("GAR_CLIENT_JSON"))
  }
  
  needed <- c("https://www.googleapis.com/auth/analytics", 
              "https://www.googleapis.com/auth/analytics.readonly",
              "https://www.googleapis.com/auth/analytics.edit",
              "https://www.googleapis.com/auth/analytics.manage.users",
              "https://www.googleapis.com/auth/analytics.user.deletion")
  
  googleAuthR::gar_attach_auto_auth(needed, 
                                    environment_var = "GARGLE_EMAIL")
  # for json files
  googleAuthR::gar_attach_auto_auth(needed, 
                                    environment_var = "GA_AUTH_FILE")
  
  invisible()
  
}

