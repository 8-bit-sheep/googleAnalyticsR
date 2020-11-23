assert_that_list <- function(the_list, assert_f){
  lapply(the_list, function(x) assert_that(assert_f(x)))
}

#' when we need a list of objs of class x to also be class x
#' @noRd
#' @importFrom methods is
assign_list_class <- function(x, the_class){
  if(!is.null(x)){
    # fix 253
    # make sure its a list of segment_ga4 objects
    if(class(x) == "list" &&
       all(unlist(lapply(x, function(y) is(y, the_class))))){
      class(x) <- the_class
    } else {
      x <- as(x, the_class)
    }
    
  }
  
  ## will be NULL if x was NULL
  x

}

#' assign new value if not null and check passes
#' @noRd
assign_new <- function(new, old, check_f = is.function){
  if(!is.null(new)){
    assert_that(check_f(new))
    return(new)
  }
  old
}

#' Function argument names
#' @noRd
function_args <- function(f, include_dots = FALSE){
  if(include_dots){
   return(names(formals(f))) 
  }
  setdiff(names(formals(f)),"...")
}

#' check package installed
#' @noRd
#' @importFrom purrr walk
check_packages_installed <- function(x, stop_if_not = TRUE, load_them = FALSE){
  if(is.null(x)){
    return()
  }
  stopifnot(is.character(x))
  
  check_one <- function(y){
    its_here <- TRUE
    if (!requireNamespace(y, quietly = TRUE)) {
      nope <- sprintf("%s needed for this function to work. Please install it via install.packages('%s')",
                      y,y)
      if(stop_if_not) stop(nope, call. = FALSE) else myMessage(nope)
      if(!stop_if_not) its_here <- FALSE
    }
    return(its_here)
  }
  
  walk(x, check_one)
  
  if(load_them){
    walk(x, library, character.only = TRUE)
  }

}

#' convert 'yesterday', 'today', 'Ndaysago etc. into R dates
#' @noRd
#' @import assertthat
#' @return a date object
process_date <- function(x){
  if(is.date(x)){
    return(x)
  }
  
  assert_that(is.character(x))
  
  tryCatch(
    return(as.Date(x)),
    error = function(err){
      NULL
    }
  )
  
  x <- tolower(x)
  
  # turn NDaysAgo into R Dates
  r_nd <- "^(.+)daysago$"

  if(grepl(r_nd, x)){
    new_date <- Sys.Date() - as.numeric(gsub(r_nd,"\\1", x))
  } else if(x == "today"){
    new_date <- Sys.Date()
  } else if(x == "yesterday"){
    new_date <- Sys.Date() - 1
  } else {
    # try to convert to date
    new_date <- as.Date(as.numeric(x), origin = "1970-01-01")
    if(is.na(new_date)){
      stop("Unrecognised date - must be 'today', 'yesterday' or 'NdaysAgo' - got: ", x, call. = FALSE)
    }

  }

  new_date
}


check_empty <- function(x){
  if(any(is.null(x), length(x) == 0)){
    myMessage("No items found", level = 3)
    return(NULL)
  } else {
    TRUE
  }
}

#' iso8601 timestamp to R
#' @noRd
iso8601_to_r <- function(x){
  as.POSIXct(gsub("\\....Z$","",x), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}

safe_extract <- function(x){
  if(is.null(x)){
    return(NA_character_)
  }
  x
}


#' data.frames within data.frames to flatten, take residual lists and concat them
#' @noRd
#' @importFrom jsonlite flatten
#' @importFrom dplyr mutate_if
#' @importFrom purrr map_chr
super_flatten <- function(x){
  x %>% 
    flatten(recursive = TRUE) %>% 
    mutate_if(is.list, ~purrr::map_chr(., paste, collapse = ","))
}

#' Nested lists with data.frames within
#' list entity names become a column
#' @noRd
# nested_flatten <- function(x){
#   x %>% 
#     map_if(is.list, function(x) ) %>% 
#     mutate_if(is.list, nested_flatten)
# }


# common paging function
get_attr_nextLink <- function(x){
  attr(x, "nextLink")
}


# assert_that_null
# do an assert_that test only if x is not null
assert_that_ifnn <- function(x, assert_f){
  if(!is.null(x)){
    assert_that(assert_f(x), 
                msg = paste(deparse(substitute(assert_f)), "failed"))
  }
}


# from https://github.com/hadley/assertthat/blob/master/R/assertions.r
is.named <- function(x) {
  nm <- names(x)
  !is.null(nm) && all(!is.na(nm) & nm != "")
}

#' if argument is NULL, no line output
#'
#' @keywords internal
#' @noRd
cat0 <- function(prefix = "", x){

  if(is.null(x)){
    invisible(return())
  } 
  
  if(inherits(x, "list") && length(x) < 1){
    invisible(return())
  }
  
  
  if(inherits(x, "list") && length(x) > 0){
    cat(prefix, "\n")
    return(print(x))
  } else {
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
#' @noRd
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
#' @noRd
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
#' @noRd
expect_null_or_s3_class <- function(thing, s3class){
  if(!is.null(thing)){
    assertthat::assert_that(inherits(thing, s3class))
  } else {
    TRUE
  }
}

#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
#' @noRd
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
#' @noRd
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

#' check it starts with ga: and if not puts it on
#'
#' @keywords internal
#' @noRd
checkPrefix <- function(x, prefix=c("ga", "mcf")){
  prefix <- match.arg(prefix)
  
  prefix_reg <- paste0("^",prefix,":")
  
  if(grepl(prefix_reg, x)) x else paste0(prefix,":",x)
}

#' Add name of list entry of dataframe to dataframe column
#'
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
error.message <- function(test_me){
  if(is.error(test_me)) attr(test_me, "condition")$message
}

#' Idempotency
#'
#' A random code to ensure no repeats
#'
#' @return A random 15 digit hash
#' @keywords internal
#' @noRd
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
#' @noRd
#' @import cli
myMessage <- function(..., level = 2){
  
  compare_level <- getOption("googleAuthR.verbose")
  
  if(level >= compare_level){
    time <- paste(Sys.time(),">")
    mm <- paste(...)
    if(grepl("^#", mm)){
      cli_h1(mm)
    } else {
      cli_div(theme = list(span.time = list(color = "grey")))
      cli_alert_info("{.time {time}} {mm}")
      cli_end()
    }
    
  }
  
}