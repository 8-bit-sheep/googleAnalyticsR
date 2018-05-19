#' Create a new goal.
#'
#' @param Goal The Goal object to be added to the view.  See examples.
#' @param accountId Account Id of the account to add the Goal to
#' @param webPropertyId Property Id of the property to add the Goal to
#' @param viewId View Id of the view to add the Goal to
#'
#' @return The Goal object
#'
#' @seealso \url{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/#Goals}
#'
#' @examples
#'
#' \dontrun{
#' 
#' ## Create a Goal object based on destination:
#' Goal <- list(
#'   id = '17',
#'   active = TRUE,
#'   name = 'Checkout',
#'   type = 'URL_DESTINATION',
#'   urlDestinationDetails = list(
#'     url = '\\/checkout\\/thank_you',
#'     matchType = 'REGEX',
#'     caseSensitive = FALSE,
#'     firstStepRequired = FALSE,
#'     steps = list(
#'       list(
#'         number = 1,
#'         name = 'Product',
#'         url = '\\/products\\/'
#'       ),
#'       list(
#'         number = 2,
#'         name = 'Cart',
#'         url = '\\/cart'
#'       ),
#'       list(
#'         number = 3,
#'         name = 'Contact',
#'         url = '\\/checkout\\/contact_information'
#'       ),
#'       list(
#'         number = 4,
#'         name = 'Shipping',
#'         url = '\\/checkout\\/shipping'
#'       ),
#'       list(
#'         number = 5,
#'         name = 'Payment',
#'         url = '\\/checkout\\/payment'
#'       ),
#'       list(
#'         number = 6,
#'         name = 'Processing',
#'         url = '\\/checkout\\/processing'
#'       )
#'     )
#'   )
#' )
#'
#' ## Create a Goal object based on an event:
#' Goal <- list(
#'   id = '9',
#'   active = TRUE,
#'   name = 'PDF Download',
#'   type = 'EVENT',
#'   eventDetails = list(
#'     useEventValue = TRUE,
#'     eventConditions = list(
#'       list(
#'         type = 'CATEGORY',
#'         matchType = 'EXACT',
#'         expression = 'PDF Download'
#'         ),
#'       list(
#'         type = 'LABEL',
#'         matchType = 'EXACT',
#'         expression = 'January brochure'
#'         )
#'       )
#'     )
#'   )
#'   
#' ## Create a Goal object based on a number of pages visitied in a session:  
#' Goal <- list(
#'   id = '10',
#'   active = TRUE,
#'   name = 'Visited more than 3 pages',
#'   type = 'VISIT_NUM_PAGES',
#'   visitNumPagesDetails = list(
#'     comparisonType = 'GREATER_THAN',
#'     comparisonValue = 3
#'   )
#' )
#'   
#' ## Create a Goal object based on the number of seconds spent on the site  
#' Goal <- list(
#'   id = '11',
#'   active = TRUE,
#'   name = 'Stayed for more than 2 minutes',
#'   type = 'VISIT_TIME_ON_SITE',
#'   visitTimeOnSiteDetails = list(
#'     comparisonType = 'GREATER_THAN',
#'     comparisonValue = 120
#'   )
#' )
#' }
#'   
#' @importFrom googleAuthR gar_api_generator
#' @import assertthat
#' @family managementAPI functions
#' @export
ga_goal_add <- function(Goal,
                        accountId,
                        webPropertyId,
                        viewId) {
  
  assert_that(is.list(Goal))
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         "POST",
                         path_args = list(
                           accounts = accountId,
                           webproperties = webPropertyId,
                           profiles = viewId,
                           goals = ""
                         ),
                         data_parse_function = function(x) x)
  
  f(the_body = Goal)
  
}


#' Updates an existing goal.
#'
#' @param Goal The Goal object to be updated
#' See examples from ga_goal_add()
#' @param accountId Account Id of the account in which to modify the Goal
#' @param webPropertyId Property Id of the property in which to modify the Goal
#' @param viewId View Id of the view in which to modify the Goal
#' @param goalId The id of the goal to be modified
#' @param method PUT by default. For patch semantics use PATCH
#' 
#' @return A goalManagement object
#' @seealso \url{https://developers.google.com/analytics/devguides/config/mgmt/v3/mgmtReference/#Goals} 
#' @examples 
#' 
#' \dontrun{
#' 
#' # Change the goal 11 to visits over 3 minutes
#' Goal <- list(
#'   active = TRUE,
#'   name = 'Stayed for more than 3 minutes',
#'   type = 'VISIT_TIME_ON_SITE',
#'   visitTimeOnSiteDetails = list(
#'     comparisonType = 'GREATER_THAN',
#'     comparisonValue = 180
#'   )
#' )
#' ga_goal_update(Goal, accountId, propertyId, viewId, 11)
#'
#' # Change destination url for goal 17
#' Goal <- list(
#'     urlDestinationDetails = list(
#'       url = '\\/checkout\\/success'
#'     )
#'   )
#' 
#' # Only the fields we're changing required because we're using PATCH method  
#' ga_goal_update(Goal, accountId, propertyId, viewId, 17, method = "PATCH")
#'   
#' }
#'   
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_goal_update <- function(Goal, 
                           accountId, 
                           webPropertyId,
                           viewId,
                           goalId, 
                           method = c("PUT","PATCH")){
  
  method <- match.arg(method)
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  f <- gar_api_generator(url,
                         method,
                         path_args = list(
                           accounts = accountId,
                           webproperties = webPropertyId,
                           profiles = viewId,
                           goals = goalId
                         ),
                         data_parse_function = function(x) x)
  
  f(the_body = Goal)
}