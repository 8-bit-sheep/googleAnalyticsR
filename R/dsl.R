ga_met <- function(x) {
  assert_that(is.string(x))
  structure(x, class = "ga_met")
}
ga_dim <- function(x) {
  assert_that(is.string(x))
  structure(x, class = "ga_dim")
}

meta_tidy <- function(){
  meta <- tibble::as_tibble(googleAnalyticsR::meta)
  meta2 <- split(meta$name, tolower(meta$type))
  meta2$dimension <- rlang::set_names(lapply(meta2$dimension, ga_dim), 
                                      gsub("^ga:", "", meta2$dimension))
  meta2$metric <- rlang::set_names(lapply(meta2$metric, ga_met), 
                                   gsub("^ga:", "", meta2$metric))
  meta2
}


is_ga_dim <- function(x) inherits(x, "ga_dim")
is_ga_met <- function(x) inherits(x, "ga_met")

filter_funs <- list(
  
  `%+%` = function(...){
    list(...)
  },
  
  `&&` = function(e1, e2) {
    filter_clause_ga4(list(e1, e2), "AND")
  },
  `||` = function(e1, e2) {
    filter_clause_ga4(list(e1, e2), "OR")
  },
  
  `==` = function(e1, e2) {
    if (is_ga_dim(e1) && is.character(e2)) {
      dim_filter(unclass(e1), "EXACT", e2)
    } else if (is_ga_dim(e1) && is.numeric(e2)) {
      dim_filter(unclass(e1), "NUMERIC_EQUAL", e2)
    } else if (is_ga_met(e1)) {
      met_filter(unclass(e1), "EQUAL", e2)
    } else {
      stop("Unsupported metric/dimension type passed")
    }
  },
  
  `!=` = function(e1, e2) {
    if (is_ga_dim(e1) && is.character(e2)) {
      dim_filter(unclass(e1), "EXACT", e2, not = TRUE)
    } else if (is_ga_dim(e1) && is.numeric(e2)) {
      dim_filter(unclass(e1), "NUMERIC_EQUAL", e2, not = TRUE)
    } else if (is_ga_met(e1)) {
      met_filter(unclass(e1), "EQUAL", e2, not = TRUE)
    } else {
      stop("Unsupported metric/dimension type passed")
    }
  },
  
  `<` = function(e1, e2) {
    if (is_ga_dim(e1)) {
      dim_filter(unclass(e1), "NUMERIC_LESS_THAN", as.character(e2))
    } else if (is_ga_met(e1)) {
      met_filter(unclass(e1), "LESS_THAN", e2)
    } else {
      stop("Invalid input", call. = FALSE)
    }
  },
  
  `!<` = function(e1, e2) {
    if (is_ga_dim(e1)) {
      dim_filter(unclass(e1), "NUMERIC_LESS_THAN", as.character(e2), not = TRUE)
    } else if (is_ga_met(e1)) {
      met_filter(unclass(e1), "LESS_THAN", e2, not = TRUE)
    } else {
      stop("Invalid input", call. = FALSE)
    }
  },
  
  `>` = function(e1, e2) {
    if (is_ga_dim(e1)) {
      dim_filter(unclass(e1), "NUMERIC_GREATER_THAN", as.character(e2))
    } else if (is_ga_met(e1)) {
      met_filter(unclass(e1), "GREATER_THAN", e2)
    } else {
      stop("Invalid input", call. = FALSE)
    }
  },
  
  `!>` = function(e1, e2) {
    if (is_ga_dim(e1)) {
      dim_filter(unclass(e1), "NUMERIC_GREATER_THAN", as.character(e2), not = TRUE)
    } else if (is_ga_met(e1)) {
      met_filter(unclass(e1), "GREATER_THAN", e2, not = TRUE)
    } else {
      stop("Invalid input", call. = FALSE)
    }
  },
  #dimension only filters
  `%in_list%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "IN_LIST", as.character(e2))
    } else {
      stop("Must use dimension for %in_list% operator")
    }
  },
  
  `%not_list%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "IN_LIST", as.character(e2), not = TRUE)
    } else {
      stop("Must use dimension for %not_list% operator")
    }
  },
  
  `~~` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "REGEXP", as.character(e2))
    } else {
      stop("Must use dimension for regexp (~~) operator")
    }
  },
  
  `!~` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "REGEXP", as.character(e2), not = TRUE)
    } else {
      stop("Must use dimension for not regexp (!~) operator")
    }
  },
  
  `%begin%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "BEGINS_WITH", as.character(e2))
    } else {
      stop("Must use dimension for %begin% operator")
    }
  },
  
  `%not_begin%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "BEGINS_WITH", as.character(e2), not = TRUE)
    } else {
      stop("Must use dimension for %begin% operator")
    }
  },
  
  `%end%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "ENDS_WITH", as.character(e2))
    } else {
      stop("Must use dimension for %end% operator")
    }
  },
  
  `%not_end%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "ENDS_WITH", as.character(e2), not = TRUE)
    } else {
      stop("Must use dimension for %end% operator")
    }
  },
  
  `%contain%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "PARTIAL", as.character(e2))
    } else {
      stop("Must use dimension for %contain% operator")
    }
  },
  
  `%not_contain%` = function(e1, e2){
    if(is_ga_dim(e1)){
      dim_filter(unclass(e1), "PARTIAL", as.character(e2), not = TRUE)
    } else {
      stop("Must use dimension for %contain% operator")
    }
  }
)

#' Create filters via the DSL
#' 
#' This lets you create filter objects using a convinient syntax, suitable to be passed to \link{google_analytics}
#' 
#' @param x The DSL expression to evaluate into a GA filter
#' 
#' @return A filter clause object
#' @export
ga_filter_dsl <- function(x) {
  meta2 <- meta_tidy()
  x <- rlang::enquo(x)
  filter_env <- rlang::as_data_mask(c(meta2$dimension, meta2$metric, filter_funs))
  o <- rlang::eval_tidy(x, data = filter_env)
  if(is.met_filter_clause(o) || is.dim_filter_clause(o)){
    return(o)
  } else {
    return(list(o))
  }
}