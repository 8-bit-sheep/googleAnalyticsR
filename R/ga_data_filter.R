#' Create a filter for use within App+Web filter expressions
#' 
#' Used within \link{ga_aw_filter_expr}
#' 
#' @param field The dimension or metric field name
#' @param value The value - filter type is based on what class of object
#' @param operation How the filter operates on field with value
#' @param caseSensitive For character based value filters
#' 
#' @details 
#' 
#' Ensure your value is of the right R class for the type of filter you desire.
#' 
#' \itemize{
#'   \item{character: string filter}
#'   \item{character vector: in list filter}
#'   \item{numeric: Numeric filter}
#'   \item{numeric 2-length vector: between filter}
#'   \item{logical: TRUE will filter for NULLs}
#'  }
#' 
#' For numerics also make sure to specify integer or float for metrics e.g.
#' 1L vs 1.00
#' 
#' Ensure operation is valid for the type of field you are passing:
#' 
#' string filters: "EXACT","BEGINS_WITH","ENDS_WITH","CONTAINS","FULL_REGEXP",
#'   "PARTIAL_REGEXP"
#'   
#' numeric filters: "EQUAL", "LESS_THAN", "LESS_THAN_OR_EQUAL", "GREATER_THAN",
#'   "GREATER_THAN_OR_EQUAL")
#'   
#'   
#' 
#' @export
#' @importFrom assertthat is.string is.count
ga_aw_filter <- function(field,
                         value, 
                         operation = c("EXACT",
                                       "BEGINS_WITH",
                                       "ENDS_WITH",
                                       "CONTAINS",
                                       "FULL_REGEXP",
                                       "PARTIAL_REGEXP",
                                       "EQUAL",
                                       "LESS_THAN",
                                       "LESS_THAN_OR_EQUAL",
                                       "GREATER_THAN",
                                       "GREATER_THAN_OR_EQUAL"),
                         caseSensitive = TRUE){
  if(is.string(value)){
    assert_that(operation %in% c("EXACT",
                                 "BEGINS_WITH",
                                 "ENDS_WITH",
                                 "CONTAINS",
                                 "FULL_REGEXP",
                                 "PARTIAL_REGEXP"))
    o <- Filter_aw(field, 
                   stringFilter = StringFilter(
                     value,
                     matchType = operation,
                     caseSensitive = caseSensitive
                   ))
  } else if(is.character(value)){
    o <- Filter_aw(field,
                   inListFilter = InListFilter(
                     value,
                     caseSensitive = caseSensitive
                   ))
  } else if(is.numeric(value)){
    assert_that(operation %in% c("EQUAL",
                                 "LESS_THAN",
                                 "LESS_THAN_OR_EQUAL",
                                 "GREATER_THAN",
                                 "GREATER_THAN_OR_EQUAL"))
    o <- Filter_aw(field,
                   numericFilter = NumericFilter(
                     value,
                     operation = operation
                   ))
  } else if(is.numeric(value) && length(value) == 2){
    o <- Filter_aw(field,
                   betweenFilter = BetweenFilter(
                     fromValue = value[[1]],
                     toValue = value[[2]]
                   ))
  } else if(is.flag(value)){
    o <- Filter_aw(field, nullFilter = value)
  } else {
    stop("Filter didn't know what to do with value of type: ", class(value), 
         call. = FALSE)
  }
  
  o
  
}

#' Create a filter expression for use with App+Web Reports
#' 
#' For use with \link{ga_data} - filter expressions are built up via filters or other nested filter expressions.
#' 
#' @param ... Vectors of \link{ga_aw_filter}, or vectors of FilterExpression created from previous calls to this function.
#' @param type The type of filter
#' 
#' @return A FilterExpression
#' 
#' @details The fields in a FilterExpression need to be either all dimensions or all metrics. Use them in the appropriate dimensionFilter or metricFilter arguments within \link{ga_data}
#' 
#' You can't pass a mix of filters and filter expressions, or multiple filter expressions with type="not" as it doesn't know how to combine the filters (and/or) - make a filter expression with type = "and/or" then pass that back into the function with type="not" to negate the filter expression.
#' 
#' @export
#' @examples 
#' 
#' simple <- ga_aw_filter_expr(ga_aw_filter("city", "Copenhagen", "EXACT"))
#' 
#' multiple <- ga_aw_filter_expr(
#'       ga_aw_filter("dayOfWeek", "4", "EXACT"),
#'      ga_aw_filter("dayOfWeek", "5", "EXACT"),
#'     type = "or")
#'    
#'# equivalent to above as uses InList filter
#' multiple_or2 <- ga_aw_filter_expr(
#'       ga_aw_filter("dayOfWeek", c("4","5")))
#' 
#' # make a filter expression then pass it again with type='not' to negate it 
#' negative <- ga_aw_filter_expr(multiple_or2, type = "not")
#' 
#' # or directly - 

#' clean_city <- ga_aw_filter_expr(
#'   ga_aw_filter("city", "(not set)", "EXACT",), type = "not")
#'
#' # use previously created FilterExpressions to build up more complex filters
#' complex <- ga_aw_filter_expr(multiple_or2, clean_city)
#' 
#'    
#' 
ga_aw_filter_expr <- function(..., 
                              type = c("default","not","and","or")){
  type <- match.arg(type)
  dots <- list(...)
  
  # a list of Filters
  areFilters <- all(vapply(dots, is.Filter, FUN.VALUE = logical(1)))
  areFilterExpressions <- all(vapply(dots, is.FilterExpression, 
                                     FUN.VALUE = logical(1)))
  
  if(areFilters) return(make_filter_expr(dots, type))
  
  if(areFilterExpressions) return(make_filter_expr_list(dots, type))
  
  stop("Inconsistent filter objects - must all be Filters or FilterExpressions", 
       call. = FALSE)
  
}

dsl_filter_expr_funcs <- list(
  
  # filter expression operators
  `|` = function(e1, e2){
    ga_aw_filter_expr(e1, e2, type = "or")
  },
  
  `&` = function(e1, e2){
    ga_aw_filter_expr(e1, e2, type = "and")
  },
  
  `!` = function(x){
    ga_aw_filter_expr(x, type = "not")
  },
  
  `==` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "EXACT")
    } else if(inherits(e2, "numeric")){
      ga_aw_filter(e1, e2, operation = "EQUAL")
    } else {
      stop("value for '==' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  # numeric only filters
  `<` = function(e1, e2){
    
    if(inherits(e2, "character")){
      stop("Can't use < (LESS_THAN) for value strings: ", e2, call. = FALSE)
    } else if(inherits(e2, "numeric")){
      ga_aw_filter(e1, e2, operation = "LESS_THAN")
    } else {
      stop("value for '<' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `>` = function(e1, e2){
    
    if(inherits(e2, "character")){
      stop("Can't use > (GREATER_THAN) for value strings: ", e2, call. = FALSE)
    } else if(inherits(e2, "numeric")){
      ga_aw_filter(e1, e2, operation = "GREATER_THAN")
    } else {
      stop("value for '>' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `<=` = function(e1, e2){
    
    if(inherits(e2, "character")){
      stop("Can't use <= (LESS_THAN_OR_EQUAL) for value strings: ", e2, call. = FALSE)
    } else if(inherits(e2, "numeric")){
      ga_aw_filter(e1, e2, operation = "LESS_THAN_OR_EQUAL")
    } else {
      stop("value for '<=' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `>=` = function(e1, e2){
    
    if(inherits(e2, "character")){
      stop("Can't use <= (GREATER_THAN_OR_EQUAL) for value strings: ", e2, call. = FALSE)
    } else if(inherits(e2, "numeric")){
      ga_aw_filter(e1, e2, operation = "GREATER_THAN_OR_EQUAL")
    } else {
      stop("value for '>=' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  # string filters
  `%begins%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "BEGINS_WITH")
    } else if(inherits(e2, "numeric")){
      stop("Can't use %begins% (BEGINS_WITH) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%begins%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  # string filters
  `%BEGINS%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "BEGINS_WITH", caseSensitive = FALSE)
    } else if(inherits(e2, "numeric")){
      stop("Can't use %BEGINS% (BEGINS_WITH, case insensitive) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%BEGINS%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%ends%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "ENDS_WITH")
    } else if(inherits(e2, "numeric")){
      stop("Can't use %ends% (ENDS_WITH) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%ends%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%ENDS%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "ENDS_WITH", caseSensitive = FALSE)
    } else if(inherits(e2, "numeric")){
      stop("Can't use %ENDS% (ENDS_WITH, case insensitive) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%ENDS%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%in%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "CONTAINS")
    } else if(inherits(e2, "numeric")){
      stop("Can't use %in% (CONTAINS) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%in%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%IN%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "CONTAINS", caseSensitive = FALSE)
    } else if(inherits(e2, "numeric")){
      stop("Can't use %IN% (IN) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%in%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%regex%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "FULL_REGEXP")
    } else if(inherits(e2, "numeric")){
      stop("Can't use %regex% (FULL_REGEXP) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for %regex% is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%REGEX%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "FULL_REGEXP", caseSensitive = FALSE)
    } else if(inherits(e2, "numeric")){
      stop("Can't use %REGEX% (FULL_REGEXP) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%REGEX%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%regex_partial%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "PARTIAL_REGEXP")
    } else if(inherits(e2, "numeric")){
      stop("Can't use %regex_partial% (PARTIAL_REGEXP) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%regex_partial%' is neither character or numeric class", 
           call. = FALSE)
    }
  },
  
  `%REGEX_PARTIAL%` = function(e1, e2){
    
    if(inherits(e2, "character")){
      ga_aw_filter(e1, e2, operation = "PARTIAL_REGEXP", caseSensitive = FALSE)
    } else if(inherits(e2, "numeric")){
      stop("Can't use %regex_partial% (PARTIAL_REGEXP) for numerics: ", e2, call. = FALSE)
    } else {
      stop("value for '%REGEX_PARTIAL%' is neither character or numeric class", 
           call. = FALSE)
    }
  }
)

#' DSL for GA4 filters
#' @param x DSL enabled syntax
#' @export
#' @importFrom rlang enquo eval_tidy
#' @examples 
#' 
#' ## filter clauses
#' # or string filter
#' ga_data_filter("city"=="Copenhagen" | "city" == "London")
#' # inlist string filter
#' ga_data_filter("city"==c("Copenhagen","London"))
#' # and string filters
#' ga_data_filter("city"=="Copenhagen" & "dayOfWeek" == "5")
#' # invert string filter
#' ga_data_filter(!("city"=="Copenhagen" | "city" == "London"))
#' 
#' ## numeric filter types
#' # numeric equal filter
#' ga_data_filter("sessions"==5)
#' # between numeric filter
#' ga_data_filter("sessions"==c(5,6))
#' # greater than numeric
#' ga_data_filter("sessions" > 0)
#' # greater than or equal
#' ga_data_filter("sessions" >= 1)
#' # less than numeric
#' ga_data_filter("sessions" < 100)
#' # less than or equal numeric
#' ga_data_filter("sessions" <= 100)
#' 
#' ## string filter types
#' # begins with string
#' ga_data_filter("city" %begins% "Cope")
#' # ends with string
#' ga_data_filter("city" %ends% "hagen")
#' # contains string
#' ga_data_filter("city" %in% "ope")
#' # regex (full) string
#' ga_data_filter("city" %regex% "^Cope")
#' # regex (partial) string
#' ga_data_filter("city" %regex_partial% "ope")
#' 
#' # by default string filters are case sensitive.  
#' # Use UPPERCASE operator to make then case insensitive
#' 
#' # begins with string (case insensitive)
#' ga_data_filter("city" %BEGINS% "cope")
#' # ends with string (case insensitive)
#' ga_data_filter("city" %ENDS% "Hagen")
#' 
#' 
ga_data_filter <- function(x){
  x <- rlang::enquo(x)
  rlang::eval_tidy(x, data = dsl_filter_expr_funcs)
}

make_filter_expr_list <- function(dots, type){
  myMessage("Got FilterExpressions", level = 2)
  # turn into a list of filterExpressionLists
  assert_that_list(dots, is.FilterExpression)
  if(type == "default"){
    type <- "and"
  }
  
  if(type == "not"){
    if(length(dots) > 1){
      stop("Invalid type='not' for multiple FilterExpressions (we don't know how to combine type=and/or) - build FilterExpression first and then pass back to ga_aw_filter_expr() with type = 'not'", call. = FALSE)
    }
    return(construct_filter_expr(dots[[1]], type = "not"))
  }
  
  assert_that(type %in% c("and","or"))
  
  filter_list <- FilterExpressionList(dots)
  
  construct_filter_expr(filter_list, type = type)
}

make_filter_expr <- function(dots, type){
  
  assert_that_list(dots, is.Filter)
  
  if(length(dots) == 1 && type %in% c("and","or")){
    warning("Ignoring argument 'type': only one filter. type=", type)
  }
  
  if(length(dots) == 1 && type == "default"){
    # a simple filter
    return(construct_filter_expr(dots[[1]], type = "default"))
  }
  
  # length > 1 of Filters - make list of filterExpressions
  lots_filters <- lapply(dots, construct_filter_expr, type = "default")
  
  make_filter_expr_list(lots_filters, type = type)
  
}



#' Constructs filterExpressions
#' @param a_filter A Filter object or a FilterExpression or a list of FilterExpressions
#' @param type The type of filter
#' @noRd
construct_filter_expr <- function(a_filter,
                                  type = c("default","not","and","or")){
  
  type <- match.arg(type)
  
  switch(type,
         default = FilterExpression(filter = a_filter),
         not = FilterExpression(notExpression = a_filter),
         and = FilterExpression(andGroup = a_filter),
         or = FilterExpression(orGroup = a_filter))
  
}




