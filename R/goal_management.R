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
#' ## Create a Goal object based on destination:
#'Goal <- list(
#'  resource = list(
#'    id = '2',
#'    active = TRUE,
#'    name = 'Checkout',
#'    type = 'URL_DESTINATION',
#'    urlDestinationDetails = list(
#'      url = '\\/checkout\\/thank_you',
#'      matchType = 'Checkout Complete',
#'      caseSensitive = FALSE,
#'      firstStepRequired = FALSE,
#'      steps = list(
#'        number = c(1,2,3,4,5,6),
#'        name = c('Product','Cart','Contact','Shipping','Payment','Processing'),
#'        url = c('.*\\/products\\/.*','\\/cart','\\/checkout\\/contact_information',
#'                '\\/checkout\\/shipping','\\/checkout\\/payment','\\/checkout\\/processing')
#'        )
#'      )
#'    )
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
