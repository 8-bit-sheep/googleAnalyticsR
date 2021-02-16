#' Order DSL for GA4 OrderBy
#' 
#' Use with [ga_data] to create orderBys
#' 
#' @param x Order DSL enabled syntax
#' @param type Order Type
#' 
#' @details 
#' 
#' The DSL rules are:
#' 
#' \itemize{
#'   \item{}{Fields can be quoted or unquoted.  If unquoted they will be validated}
#'   \item{}{Use + as a prefix to indicate ascending order e.g. `+sessions`}
#'   \item{}{Use - as a prefix to indicate decreasing order e.g. `-sessions`}
#'   \item{}{Combine order fields without commas e.g. `+sessions -city`}
#'   \item{}{Ordering of dimensions can also specify a type of ordering: ALPHANUMERIC, CASE_INSENSITIVE_ALPHANUMERIC, NUMERIC}
#' }
#' 
#' The dimension ordering have these effects:
#' 
#' \itemize{
#'   \item{ALPHANUMERIC}{ For example, "2" < "A" < "X" < "b" < "z"}
#'   \item{CASE_INSENSITIVE_ALPHANUMERIC}{ Case insensitive alphanumeric sort by lower case Unicode code point. For example, "2" < "A" < "b" < "X" < "z"}
#'   \item{NUMERIC}{ Dimension values are converted to numbers before sorting. For example in NUMERIC sort, "25" < "100", and in ALPHANUMERIC sort, "100" < "25". Non-numeric dimension values all have equal ordering value below all numeric values}
#' }
#' 
#' 
#' @return A list of [OrderBy] objects suitable for use in [ga_data]
#' @export
#' @seealso <https://developers.google.com/analytics/devguides/reporting/data/v1/rest/v1alpha/OrderBy>
#' @importFrom rlang enquo eval_tidy
#' @examples 
#' 
#' 
#' # session in descending order
#' ga_data_order(-sessions)
#' 
#' # city dimension in ascending alphanumeric order
#' ga_data_order(+city)
#' 
#' # as above plus sessions in descending order
#' ga_data_order(+city -sessions)
#' 
#' # as above plus activeUsers in ascending order
#' ga_data_order(+city -sessions +activeUsers)
#' 
#' # dayOfWeek dimension in ascending numeric order
#' ga_data_order(+dayOfWeek, type = "NUMERIC")
#' 
#' # you can also combine them with c()
#' a <- ga_data_order(-sessions)
#' b <- ga_data_order(-dayOfWeek, type = "NUMERIC")
#' c(a, b)
#' 
#' \dontrun{
#' # example of use
#' ga_data(
#'   206670707,
#'   metrics = c("activeUsers","sessions"),
#'   dimensions = c("date","city","dayOfWeek"),
#'   date_range = c("2020-03-31", "2020-04-27"),
#'   orderBys = ga_data_order(-sessions -dayOfWeek)
#'   )
#' 
#' 
#' }
#' 
#' 
#' @family GA4 functions
ga_data_order <- function(x, type = c("ALPHANUMERIC",
                                      "CASE_INSENSITIVE_ALPHANUMERIC",
                                      "NUMERIC")){
  type <- match.arg(type)
  x <- rlang::enquo(x)
  
  mask_data <- c(dsl_order_expr_funcs(type), 
                 filter_validation_meta())

  rlang::eval_tidy(x, data = mask_data)
}

dsl_order_expr_funcs <- function(orderType){
  list(
  
  `+` = function(x,y=NULL){
    
    out <- NULL
    # so +g +h doesn't become (+g) + h 
    if(is.OrderBy(x[[1]])){
      out <- x
      x <- y
    }
    myMessage("orderBy +:  x", x," y:", y)
    type <- lookup_field_class(x)

    o <- switch(type,
      metric    = OrderBy(metric = x, desc = FALSE),
      dimension = OrderBy(
        dimension = DimensionOrderBy(x, 
                                     orderType = orderType), 
        desc = FALSE)
      )
    
    if(!is.null(out)){
      # recurse
      return(c(out, list(o)))
    }
  
    list(o)
  } ,
  
  `-` = function(x,y=NULL){
    
    out <- NULL
    # so +g +h doesn't become (+g) + h 
    if(is.OrderBy(x[[1]])){
      out <- x
      x <- y
    }
    myMessage("orderBy -:  x", x," y:", y)
    type <- lookup_field_class(x)
    
    o <- switch(type,
                metric    = OrderBy(metric = x, desc = TRUE),
                dimension = OrderBy(
                  dimension = DimensionOrderBy(x, 
                                               orderType = orderType), 
                  desc = TRUE)
    )
    
    if(!is.null(out)){
      # recurse
      return(c(out, list(o)))
    }
    
    list(o)
  }
  
)}

lookup_field_class <- function(field){
  # use default dims
  if(is.null(.ga_meta_env$meta)){
    fields <- googleAnalyticsR::meta4
  } else {
    fields <- .ga_meta_env$meta
  }
  
  fields[fields$apiName == field,"class"]
}


#' OrderBy Object
#'
#' @details
#' The sort options.
#'
#' @param metric Sorts results by a metric's values
#' @param pivot Sorts results by a metric's values within a pivot column group
#' @param dimension Sorts results by a dimension's values
#' @param desc If true, sorts by descending order
#'
#' @return OrderBy object
#'
#' @family OrderBy functions
#' @noRd
OrderBy <- function(metric = NULL, 
                    pivot = NULL, 
                    dimension = NULL, 
                    desc = TRUE){
  assert_that(is.flag(desc))
  
  only_one <- !c(is.null(metric),
                 is.null(pivot),
                 is.null(dimension))
  assert_that(is.flag(only_one[only_one]))  
  
  assert_that_ifnn(metric, is.string)
  assert_that_ifnn(pivot, is.PivotOrderBy)
  assert_that_ifnn(dimension, is.DimensionOrderBy)
  
  the_metric <- NULL
  if(!is.null(metric)){
    the_metric <- list(metricName = metric)
  }
  
  structure(list(metric = the_metric, 
                 pivot = pivot, 
                 dimension = dimension, 
                 desc = desc),
            class = c("gar_OrderBy", "list"))
}

is.OrderBy <- function(x){
  inherits(x, "gar_OrderBy")
}

#' DimensionOrderBy Object
#'
#' @details
#' Sorts by dimension values.
#' 
#' `orderType` has values:
#' 
#' Alphanumeric sort by Unicode code point. For example, "2" < "A" < "X" < "b" < "z".
#' 
#' Case insensitive alphanumeric sort by lower case Unicode code point. For example, "2" < "A" < "b" < "X" < "z".
#' 
#' Numeric: Dimension values are converted to numbers before sorting. For example in NUMERIC sort, "25" < "100", and in ALPHANUMERIC sort, "100" < "25". Non-numeric dimension values all have equal ordering value below all numeric values.
#' 
#'
#' @param orderType Controls the rule for dimension value ordering
#' @param dimensionName A dimension name in the request to order by
#'
#' @return DimensionOrderBy object
#'
#' @family DimensionOrderBy functions
#' @noRd
DimensionOrderBy <- function(dimensionName, 
                             orderType = c("ALPHANUMERIC",
                                           "CASE_INSENSITIVE_ALPHANUMERIC",
                                           "NUMERIC")){
  orderType <- match.arg(orderType)
  assert_that(is.string(dimensionName))
  
  structure(list(orderType = orderType, 
                 dimensionName = dimensionName), 
            class = c("gar_DimensionOrderBy","list"))
}

is.DimensionOrderBy <- function(x){
  inherits(x, "gar_DimensionOrderBy")
}
