is_default_project <- function(){
  
  # if no service auth then its not using default client.id #324
  if(googleAuthR::gar_has_token()){
    token <- googleAuthR::gar_token()
    if(!is.null(token$auth_token$secrets) &&
       token$auth_token$secrets$type == "service_account") return(FALSE)
  }

  # if set web json then its shiny #333
  if(!is.null(getOption("googleAuthR.webapp.client_id")) &&
     getOption("googleAuthR.webapp.client_id") != ""){
    return(FALSE)
  }
  
  getOption("googleAuthR.client_id") %in% c("289759286325-da3fr5kq4nl4nkhmhs2uft776kdsggbo.apps.googleusercontent.com",
                                            "289759286325-42j8nmkeq5n9v9eb1kiuj2i97v9oea1f.apps.googleusercontent.com",
                                            "201908948134-rm1ij8ursrfcbkv9koc0aqver84b04r7.apps.googleusercontent.com",
                                            "201908948134-cjjs89cffh3k429vi7943ftpk3jg36ed.apps.googleusercontent.com",
                                            "289759286325-i5kd45j7qnoc1t8h86611b38icnfk38d.apps.googleusercontent.com",
                                            "289759286325-h15qtntnnounclih9tnfdj8ft1dlh1n1.apps.googleusercontent.com")
}

default_project_message <- function(){

  if(is_default_project()){
    myMessage("Default Google Project for googleAnalyticsR is set. 
      This is shared with all googleAnalyticsR users. If making a lot of API calls, please visit: 
      https://code.markedmondson.me/googleAnalyticsR/articles/setup.html#professional-mode-your-own-google-project
      for instructions on setting your own Google Project", 
              level = 3)

  }
  

}

# custom error messages for googleAnalyticsR
# otherwise googleAuthR handles them
custom_error <- function(err){
  if(grepl("insufficient tokens for quota",err$message)){
    default_project_message()
    stop("The Google Project ", getOption("googleAuthR.client_id") ," has run out of quota (typically 50,000 API calls per day)", call. = FALSE)
  } else {
    stop(err$message, call. = FALSE)
  }
}