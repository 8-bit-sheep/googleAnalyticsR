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
#'  )
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
                         ))
  
  out <- f(the_body = Goal)
  
}
