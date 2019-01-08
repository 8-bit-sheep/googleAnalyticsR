
default_project_message <- function(){
  is_default_project <- 
    getOption("googleAuthR.client_id") %in% c("289759286325-da3fr5kq4nl4nkhmhs2uft776kdsggbo.apps.googleusercontent.com",
                                              "289759286325-42j8nmkeq5n9v9eb1kiuj2i97v9oea1f.apps.googleusercontent.com",
                                              "201908948134-rm1ij8ursrfcbkv9koc0aqver84b04r7.apps.googleusercontent.com",
                                              "201908948134-cjjs89cffh3k429vi7943ftpk3jg36ed.apps.googleusercontent.com")
  if(is_default_project){
    myMessage("Default Google Project for googleAnalyticsR is now set.  \n This is shared with all googleAnalyticsR users. \n If making a lot of API calls, please: \n visit: https://bit.ly/2Evk6hn \n for instructions on setting your own Google Project ", level = 3)
  }

}

error_check <- function(x){
  if(is.error(x)){
    if(grepl("insufficient tokens for quota",error.message(x))){
      default_project_message()
      stop("The Google Project ", getOption("googleAuthR.client_id") ," has run out of quota (typically 50,000 API calls per day)", call. = FALSE)
    }
    # no need as googleAuthR will surface this error
    #stop(error.message(x))
  }
  
  x
}