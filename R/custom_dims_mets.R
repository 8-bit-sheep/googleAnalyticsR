#' Create named list of allowed GA metrics/dimensions
#' 
#' @param type Type of parameter to create
#' @param subType to restrict to only those in this type
#' @param callAPI This will update the meta table (Requires online authorization)
#' 
#' This is useful to expand goalXCompletions to all the possibilities,
#'   as well as restricting to those that variables that work with your API call.
#' 
#' Use internal meta table, but you have option to update to the latest version.
#'   
#' @return A named list of parameters for use in API calls
#' @export
ga_allowed_metric_dim <- function(type = c("METRIC", "DIMENSION"),
                                  subType = c("all","segment","cohort"),
                                  callAPI = FALSE){
  allowed_metric_dim(type=type,
                     subType=subType,
                     callAPI = callAPI)
}


allowed_metric_dim <- function(type = c("METRIC", "DIMENSION"),
                               subType = c("all","segment","cohort"),
                               callAPI = FALSE){
  type <- match.arg(type)
  subType <- match.arg(subType)
  
  if(callAPI){
    meta <- ga_meta()
  } else {
    meta <- googleAnalyticsR::meta
  }
  
  ## where to restrict what dims/metrics are chosen from, todo
  filtered_md <- switch(subType,
                        all = meta,
                        segment = meta[grepl("true",meta$allowedInSegments),],
                        cohort = meta[grepl("Lifetime Value and Cohort", meta$group),])
  
  ## only public (not deprecated) variables
  filtered_md <- filtered_md[filtered_md$type == type & 
                               filtered_md$status == "PUBLIC",]
  
  ## replace XX with 1 to 20
  meta_ex <- filtered_md[grepl("XX",filtered_md$name),]
  
  f <- function(y) vapply(1:200, 
                          function(x) gsub("XX", x, meta_ex$name[y]), character(1))
  
  meta_expanded <- unlist(lapply(seq_along(meta_ex$name), f))
  
  ## repeat with names
  f2 <- function(y) vapply(1:200, 
                           function(x) gsub("XX", x, meta_ex$uiName[y]), character(1))
  
  meta_expanded_names <- unlist(lapply(seq_along(meta_ex$uiName), f2))
  
  
  ## take out XX from filtered_md
  out <- c(filtered_md$name[!filtered_md$name %in% meta_ex$name],
           meta_expanded)
  names(out) <- c(filtered_md$uiName[!filtered_md$uiName %in% meta_ex$uiName],
                  meta_expanded_names)
  
  out
}


#' Get Custom Dimensions or Metrics
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param type A customMetric or customDimension
#' @param customId The customMetricId or customDimensionId
#'
#' @return Custom Metric or Dimension meta data
#' @importFrom googleAuthR gar_api_generator
#' @family custom variable functions
#' @export
ga_custom_vars <- function(accountId,
                           webPropertyId,
                           type = c("customMetrics", "customDimensions"),
                           customId){
  
  type <- match.arg(type)
  
  pa <- switch(type,
               customMetrics =  list(
                 accounts = accountId,
                 webproperties = webPropertyId,
                 customMetrics = customId
               ),
               customDimensions = list(
                 accounts = accountId,
                 webproperties = webPropertyId,
                 customDimensions = customId
               )
  )
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  cus_var <- gar_api_generator(url,
                               "GET",
                               path_args = pa,
                               data_parse_function = function(x) x)
  
  res <- cus_var()
  
  as.customDimension(res)
}

#' List Custom Dimensions or Metrics
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param type A customMetric or customDimension
#'
#' @return Custom Metric or Dimension List
#' @importFrom googleAuthR gar_api_generator
#' @family custom variable functions
#' @export
#' 
#' @details 
#' 
#' This function lists all the existing custom dimensions or metrics for the web property. 
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' ga_custom_vars_list(54019251, webPropertyId = "UA-54019251-4", type = "customDimensions")
#' 
#' ga_custom_vars_list(54019251, webPropertyId = "UA-54019251-4", type = "customMetrics")
#' 
#' }
ga_custom_vars_list <- function(accountId,
                                webPropertyId,
                                type = c("customDimensions","customMetrics")){
  
  type <- match.arg(type)
  
  pa <- switch(type,
      customMetrics =  list(
        accounts = accountId,
        webproperties = webPropertyId,
        customMetrics = ""
      ),
      customDimensions = list(
        accounts = accountId,
        webproperties = webPropertyId,
        customDimensions = ""
      )
    )
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  cus_var <- gar_api_generator(url,
                               "GET",
                               path_args = pa,
                               data_parse_function = parse_ga_custom_vars_list)

  pages <- gar_api_page(cus_var, page_f = get_attr_nextLink)
  
  Reduce(bind_rows, pages)
  
}

#' @noRd
#' @import assertthat
parse_ga_custom_vars_list <- function(x){
  
  o <- x %>% 
    management_api_parsing(c("analytics#customDimensions", 
                             "analytics#customMetrics")) 

  if(is.null(o)){
    return(data.frame())
  }
  
  o <- o %>% 
    select(-parentLink.type, -parentLink.href) %>% 
    mutate(created = iso8601_to_r(created),
           updated = iso8601_to_r(updated))
  
  attr(o, "nextLink") <- x$nextLink
  o
  
}

#' Create a custom dimension
#' 
#' Create a dimension by specifying its attributes.
#' 
#' @param active \code{TRUE} or \code{FALSE} if custom dimension is active or not
#' @param index Index of custom dimension - integer between 1 and 20 (200 for GA360)
#' @param name Name of custom dimension
#' @param scope Scope of custom dimension - one of \code{"HIT","SESSION","USER","PRODUCT"}
#' @param accountId AccountId of the custom dimension
#' @param webPropertyId WebPropertyId of the custom dimension
#' 
#' @seealso \href{https://support.google.com/analytics/answer/2709828}{Custom dimensions support article}
#' @export
#' @importFrom googleAuthR gar_api_generator
#' @family custom variable functions
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' # create custom var
#' ga_custom_vars_create("my_custom_dim",
#'                       index = 15,
#'                       accountId = 54019251,
#'                       webPropertyId = "UA-54019251-4",
#'                       scope = "HIT",
#'                       active = FALSE)
#' 
#' # view custom dimension in list
#' ga_custom_vars_list(54019251, webPropertyId = "UA-54019251-4", type = "customDimensions")
#' 
#' }
ga_custom_vars_create <- function(name, 
                                  index,
                                  accountId,
                                  webPropertyId,
                                  active,
                                  scope = c("HIT","SESSION","USER","PRODUCT")){
  scope <- match.arg(scope)
  accountId <- as.character(accountId)
  
  cd <- customDimension(name = name,
                        index = index,
                        accountId = accountId,
                        webPropertyId = webPropertyId,
                        scope = scope,
                        active = active)

  api <- gar_api_generator("https://www.googleapis.com/analytics/v3/management",
                           "POST",
                           path_args = list(
                             accounts = accountId,
                             webproperties = webPropertyId,
                             customDimensions = ""
                           ),
                           data_parse_function = function(x) x)
  api(the_body = cd)
  
  
}

#' Modify a custom dimension
#' 
#' Modify existing custom dimensions
#' 
#' @param active \code{TRUE} or \code{FALSE} if custom dimension is active or not
#' @param name Name of custom dimension
#' @param scope Scope of custom dimension - one of \code{"HIT","SESSION","USER","PRODUCT"}
#' @param accountId AccountId of the custom dimension
#' @param webPropertyId WebPropertyId of the custom dimension
#' @param id The id of the custom dimension
#' @param ignoreCustomDataSourceLinks Force the update and ignore any warnings related to the custom dimension being linked to a custom data source / data set.
#' 
#' @seealso \href{https://support.google.com/analytics/answer/2709828}{Custom dimensions support article}
#' @export
#' @import assertthat
#' @importFrom googleAuthR gar_api_generator
#' @family custom variable functions
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' library(googleAnalyticsR)
#' ga_auth()
#' 
#' # create custom var
#' ga_custom_vars_create("my_custom_dim",
#'                       index = 7,
#'                       accountId = 54019251,
#'                       webPropertyId = "UA-54019251-4",
#'                       scope = "HIT",
#'                       active = FALSE)
#' 
#' # view custom dimension in list
#' ga_custom_vars_list(54019251, webPropertyId = "UA-54019251-4", type = "customDimensions")
#' 
#' # change a custom dimension
#' ga_custom_vars_patch("ga:dimension7",
#'                      accountId = 54019251,
#'                      webPropertyId = "UA-54019251-4",
#'                      name = "my_custom_dim2",
#'                      active = TRUE)
#'                      
#' # view custom dimensions again to see change
#' ga_custom_vars_list(54019251, webPropertyId = "UA-54019251-4", type = "customDimensions")
#' 
#' }
ga_custom_vars_patch <- function(id,
                                 accountId,
                                 webPropertyId,
                                 name = NULL, 
                                 active = NULL,
                                 scope = NULL,
                                 ignoreCustomDataSourceLinks = FALSE){
  
  accountId <- as.character(accountId)
  assert_that(is.string(id))
  
  if(!is.null(scope)){
    assert_that(is.string(scope),
                scope %in% c("HIT","SESSION","USER","PRODUCT"))
  }
  
  
  cd <- customDimension(name = name,
                        scope = scope,
                        active = active,
                        accountId = accountId,
                        webPropertyId = webPropertyId)
  
  cd <- rmNullObs(cd)
  
  api <- gar_api_generator("https://www.googleapis.com/analytics/v3/management/",
                           "PATCH",
                           path_args = list(accounts = accountId,
                                            webproperties = webPropertyId,
                                            customDimensions = id),
                           pars_args = list(
                             ignoreCustomDataSourceLinks = ignoreCustomDataSourceLinks
                             ),
                           data_parse_function = function(x) x)
  res <- api(the_body = cd)
  as.customDimension(res)
}


#' A customDimension object
#' 
#' Used within \link{ga_custom_vars_create} and \link{ga_custom_var_patch}
#' 
#' @param active \code{TRUE} or \code{FALSE} if custom dimension is active or not
#' @param index Index of custom dimension - integer between 1 and 20 (200 for GA360)
#' @param name Name of custom dimension
#' @param scope Scope of custom dimension - one of \code{"HIT","SESSION","USER","PRODUCT"}
#' @param accountId AccountId of the custom dimension
#' @param webPropertyId WebPropertyId of the custom dimension
#' @param id The id of the custom dimension
#' 
#' @details 
#'  When creating a new custom dimension, supply all the arguments except id.  
#'  
#'  If you are updating an existing custom dimension, only supply the arguments you wish to change
#' 
#' @family custom variable functions
#' @import assertthat
#' @noRd
customDimension <- function(name = NULL, 
                            index = NULL,
                            scope = NULL, 
                            active = NULL,
                            id = NULL,
                            accountId = NULL,
                            webPropertyId = NULL){
  
  assert_that_ifnn(name, is.string)
  assert_that_ifnn(scope, is.string)
  assert_that_ifnn(active, is.flag)
  assert_that_ifnn(id, is.string)
  
  if(!is.null(index)){
    index <- as.numeric(index)
    assert_that(is.scalar(index),
                index <= 200,
                index > 0)
  }
  
  structure(
    list(id = id,
         name = name,
         index = index,
         scope = scope,
         active = active,
         accountId = accountId,
         webPropertyId = webPropertyId),
    class = "customDimension_ga"
  )
}

is.customDimension <- function(x){
  inherits(x, "customDimension_ga")
}

as.customDimension <- function(x){
  class(x) <- c("customDimension_ga", class(x))
  x
}
