#' Test S3 class in a list
#' 
#' @param listthing A list of things
#' @param types A vector of types we want
#' @param null_ok Is it ok to have a NULL listhing?
#' 
expect_list_of_this <- function(listthing, types, null_ok=FALSE){
  
  if(null_ok){
    expect_null_or_type(listthing, "list")
    return()
  } else {
    testthat::expect_type(listthing, "list")
  }

  res <- mapply(function(thing, type) {
    class(thing)==type
    }, listthing, types)
  
  if(!any(res)){
    str(res)
    str(listthing)
    str(types)
    stop(paste(types, collapse = " "), " is not found in list")
  }
  
}


#' Expect NULL or type
#' 
#' wraps testthat::expect_type() to run if not NULL
expect_null_or_type <- function(thing, type){
  if(!is.null(thing)){
    testthat::expect_type(thing, type)
  }
}


#' @importFrom magrittr %>%
#' @export
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

#' Add name of list entry of dataframe to dataframe colum
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


