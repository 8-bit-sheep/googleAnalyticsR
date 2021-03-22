.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googleAnalyticsR <- list(
    ## default Google project
    googleAuthR.client_id = "289759286325-i5kd45j7qnoc1t8h86611b38icnfk38d.apps.googleusercontent.com",
    googleAuthR.client_secret = "RnKpRn0ZOrKbwwSPeX4Giujf",
    googleAuthR.scopes.selected = 
      c("https://www.googleapis.com/auth/analytics",
        "https://www.googleapis.com/auth/analytics.readonly",
        "https://www.googleapis.com/auth/analytics.edit",
        "https://www.googleapis.com/auth/analytics.manage.users",
        "https://www.googleapis.com/auth/analytics.user.deletion"),
    googleAuthR.quotaUser = Sys.info()[["user"]]
  )
  
  toset <- !(names(op.googleAnalyticsR) %in% names(op))
  ## only set those not set already
  if(any(toset)) options(op.googleAnalyticsR[toset])
  
  ## override existing options
  options(
    googleAuthR.batch_endpoint = 
      "https://www.googleapis.com/batch/analytics/v3",
    googleAuthR.tryAttempts = 1)
  
  f <- function(req){

    # a realtime API call - no cache
    if(!is.null(req$content$kind) && 
       req$content$kind == "analyticsData#runRealtimeReport"){
      return(FALSE)
    }
    ga4  <- tryCatch(req$content$reports, error = function(x) NULL)
    data <- tryCatch(req$content$rows, error = function(x) NULL)

    # v4 data is not golden
    if(!is.null(ga4[[1]]$data$isDataGolden) && 
       !ga4[[1]]$data$isDataGolden){
      return(FALSE)
    }
    
    # present only if including today's data? e.g. not golden?
    if(!is.null(ga4[[1]]$data$dataLastRefreshed)){
      return(FALSE)
    }

    if(!is.null(ga4) || !is.null(data)) return(TRUE)
    
    FALSE
    
  }

  googleAuthR::gar_cache_setup(invalid_func = f)
  
  invisible(NULL)
  
}

.onAttach <- function(libname, pkgname){
  
  if(nzchar(Sys.getenv("GAR_CLIENT_JSON"))){
    googleAuthR::gar_set_client(json = Sys.getenv("GAR_CLIENT_JSON"))
  }
  
  needed <- c("https://www.googleapis.com/auth/analytics", 
              "https://www.googleapis.com/auth/analytics.readonly",
              "https://www.googleapis.com/auth/analytics.edit",
              "https://www.googleapis.com/auth/analytics.manage.users",
              "https://www.googleapis.com/auth/analytics.user.deletion")
  
  # for json files
  googleAuthR::gar_attach_auto_auth(needed, 
                                    environment_var = "GA_AUTH_FILE")
  if(!file.exists(.trackme$filepath) && interactive()){
    packageStartupMessage(
      "You can opt in to tracking of your use of googleAnalyticsR - see ?ga_trackme for details."
    )
  }

  ga_trackme_event()
  
  invisible(NULL)
  
}

