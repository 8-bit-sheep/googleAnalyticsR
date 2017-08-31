# from https://github.com/hadley/assertthat/blob/master/R/assertions.r
is.named <- function(x) {
  nm <- names(x)
  !is.null(nm) && all(!is.na(nm) & nm != "")
}

#' Timestamp to R date
#' @keywords internal
timestamp_to_r <- function(t){
  as.POSIXct(t, format = "%Y-%m-%dT%H:%M:%S")
}

#' if argument is NULL, no line output
#'
#' @keywords internal
cat0 <- function(prefix = "", x){
  if(!is.null(x)){
    cat(prefix, x, "\n")
  }
}



#' Allow unit lists
#' 
#' If you need a list but only get one element, make it a list.
#' 
#' Makes it easier for users to use some functions by not worrying about list wrapping. 
#' 
#' @param perhaps_list A list or an element that will be wrapped in list()
#' 
#' @return A list
#' @keywords internal
unitToList <- function(perhaps_list){
  
  if(is.null(perhaps_list)){
    return(NULL)
  }
  
  if(inherits(perhaps_list, "list")){
    out <- perhaps_list
  } else {
    if(length(perhaps_list) == 1){
      out <- list(perhaps_list)
    } else {
      stop("Needs to be a list or a length 1 object")
    }

  }
  
  out
}

#' Expect NULL or type
#' 
#' wraps assertthat::assert_that to run if not NULL
#' @keywords internal
expect_null_or_type <- function(thing, type){
  if(!is.null(thing)){
    assertthat::assert_that(inherits(thing, type))
  } else {
    TRUE
  }
}

#' Expect NULL or class (s3)
#' 
#' wraps assertthat::assert_that(inherits(thing, s3class))() to run if not NULL
#' @keywords internal
expect_null_or_s3_class <- function(thing, s3class){
  if(!is.null(thing)){
    assertthat::assert_that(inherits(thing, s3class))
  } else {
    TRUE
  }
}


#' @importFrom magrittr %>%
#' @export
#' @keywords internal
magrittr::`%>%`

#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

#' check it starts with ga: and if not puts it on
#'
#' @keywords internal
checkPrefix <- function(x, prefix=c("ga", "mcf")){
  prefix <- match.arg(prefix)
  
  prefix_reg <- paste0("^",prefix,":")
  
  if(grepl(prefix_reg, x)) x else paste0(prefix,":",x)
}

#' Add name of list entry of dataframe to dataframe column
#'
#' @keywords internal
listNameToDFCol <- function(named_list, colName = "listName"){
  lapply(names(named_list),
         function(x) {named_list[[x]][colName] <- x
         named_list[[x]]
         })
}

#' Is this a try error?
#'
#' Utility to test errors
#'
#' @param test_me an object created with try()
#'
#' @return Boolean
#'
#' @keywords internal
is.error <- function(test_me){
  inherits(test_me, "try-error")
}

#' Get the error message
#'
#' @param test_me an object that has failed is.error
#'
#' @return The error message
#'
#' @keywords internal
error.message <- function(test_me){
  if(is.error(test_me)) attr(test_me, "condition")$message
}

#' Idempotency
#'
#' A random code to ensure no repeats
#'
#' @return A random 15 digit hash
#' @keywords internal
idempotency <- function(){
  paste(sample(c(LETTERS, letters, 0:9), 15, TRUE),collapse="")
}


#' Custom message log level
#' 
#' @param ... The message(s)
#' @param level The severity
#' 
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
#' @keywords internal
myMessage <- function(..., level = 2){
  
  compare_level <- getOption("googleAuthR.verbose")
  
  if(level >= compare_level){
    message(Sys.time() ,"> ", ...)
  }
  
}