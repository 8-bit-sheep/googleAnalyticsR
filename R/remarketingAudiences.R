#' Get a remarketing audience
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#' @param remarketingAudienceId The ID of the remarketing audience to retrieve. 
#'
#' @return Remarketing Audience object
#' @seealso \href{https://support.google.com/analytics/answer/2611268?hl=en}{About remarketing audiences}
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_remarketing_get <- function(accountId,
                               webPropertyId,
                               remarketingAudienceId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  rm <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               remarketingAudiences = remarketingAudienceId
                             ),
                             data_parse_function = function(x) x)
  
  rm()
  
}

#' List remarketing audiences
#'
#' @param accountId Account Id
#' @param webPropertyId Web Property Id
#'
#' @return Remarketing audience list
#' @seealso \href{https://support.google.com/analytics/answer/2611268?hl=en}{About remarketing audiences}
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_remarketing_list <- function(accountId,
                                webPropertyId){
  
  url <- "https://www.googleapis.com/analytics/v3/management/"
  rms <- gar_api_generator(url,
                             "GET",
                             path_args = list(
                               accounts = accountId,
                               webproperties = webPropertyId,
                               remarketingAudiences = ""
                             ),
                             data_parse_function = function(x) x)
  
  rms()
  
}

#' Estimate number of users added to the segment yesterday
#' 
#' @param remarketingAudience A remarketing audience object from \link{ga_remarketing_get}
#' 
#' Takes the segment definition from a remarketing audiences and runs 
#'   it against the viewId to see current estimated users
#' 
#' The total audience size is this figure for every membershipDurationDay from yesterday
#'
#' @return data.frame
#' @seealso \href{https://support.google.com/analytics/answer/2611268?hl=en}{About remarketing audiences}
#' @importFrom googleAuthR gar_api_generator
#' @family managementAPI functions
#' @export
ga_remarketing_estimate <- function(remarketingAudience){
  
  stopifnot(remarketingAudience$kind == "analytics#remarketingAudience")
  
  defs <- remarketingAudience$audienceDefinition$includeConditions
  
  yesterday <- Sys.Date() - 1
  google_analytics_3(
    remarketingAudience$linkedViews,
    start = yesterday - defs$daysToLookBack, end = yesterday,
    metrics = "users",
    segment = defs$segment
  )
}

#' Create a remarketing audience for creation
#' 
#' Create definitions to be used within \link{ga_remarketing_create}
#' 
#' @param segment The defintion of the segment (v3 syntax)
#' @param membershipDurationDays Number of days (in the range 1 to 540) a user remains in the audience.
#' @param daysToLookBack The look-back window lets you specify a time frame for evaluating the behavior that qualifies users for your audience.
#' @param state_duration If to be used in a state based audience, whether to make the segment temporary or permanent.
#' 
#' 
#' @details 
#' 
#' The look-back window lets you specify a time frame for evaluating the behavior that qualifies users for your audience. For example, if your filters include users from Central Asia, and Transactions Greater than 2, and you set the look-back window to 14 days, then any user from Central Asia whose cumulative transactions exceed 2 during the last 14 days is added to the audience.
#' 
#' @export
#' @import assertthat
#' @examples 
#' 
#' \dontrun{
#' adword_list <- ga_adwords_list(123456, "UA-123456-1")
#' 
#' adword_link <- ga_adword(adword_list$id[[1]])
#' 
#' segment_list <- ga_segment_list()$items$definition
#' 
#' my_remarketing1 <- ga_remarketing_build(segment_list[[1]], 
#'                     state_duration = "TEMPORARY",
#'                     membershipDurationDays = 90, 
#'                     daysToLookBack = 14)
#'                     
#' my_remarketing2 <- ga_remarketing_build(segment_list[[2]], 
#'                      state_duration = "PERMANENT",
#'                      membershipDurationDays = 7, 
#'                      daysToLookBack = 31)
#'                      
#' # state based only can include exclusions
#' ga_remarketing_create(adwords_link = adword_link,
#'                      include = my_remarketing1, 
#'                      exclude = my_remarketing2,
#'                      audienceType = "STATE_BASED", 
#'                      name = "my_remarketing_seg1")

#' 
#' }
ga_remarketing_build <- function(segment, 
                                 membershipDurationDays = NULL, 
                                 daysToLookBack = NULL, 
                                 state_duration = c("TEMPORARY","PERMANENT")){
  
  state_duration <- match.arg(state_duration)
  assert_that(is.string(segment))
  assert_that_ifnn(membershipDurationDays, is.scalar)
  assert_that_ifnn(daysToLookBack, is.scalar)
  
  structure(
    list(
      conditions = segment,
      membershipDurationDays = membershipDurationDays,
      daysToLookBack = daysToLookBack
    ), 
    class = c("ga4_remarketing_segment","list")
  )
}

is.ga4_remarketing_segment <- function(x) inherits(x, "ga4_remarketing_segment")

#' Create a new remarketing audience
#' 
#' Create a remarketing audiences built via \link{ga_remarketing_build}
#' 
#' @param adwordsLinkId The adwords link to add the remarketing audience to
#' @param include A \code{ga4_remarketing_segment} object to include via \link{ga_remarketing_build}
#' @param exclude If \code{audienceType="STATE_BASED"}, a \code{ga4_remarketing_segment} object to exclude via \link{ga_remarketing_build}
#' @param audienceType \code{SIMPLE} or \code{STATE_BASED}
#' @param name An optional name, if not supplied one will be generated
#' 
#' @details 
#' 
#' This builds and calls the API to create the remarketing audience based on the segments you have defined.
#' 
#' @export
#' @import assertthat
#' @examples 
#' 
#' \dontrun{
#' adword_list <- ga_adwords_list(123456, "UA-123456-1")
#' 
#' adword_link <- ga_adword(adword_list$id[[1]])
#' 
#' segment_list <- ga_segment_list()$items$definition
#' 
#' my_remarketing1 <- ga_remarketing_build(segment_list[[1]], 
#'                     state_duration = "TEMPORARY",
#'                     membershipDurationDays = 90, 
#'                     daysToLookBack = 14)
#'                     
#' my_remarketing2 <- ga_remarketing_build(segment_list[[2]], 
#'                      state_duration = "PERMANENT",
#'                      membershipDurationDays = 7, 
#'                      daysToLookBack = 31)
#'                      
#' # state based only can include exclusions
#' ga_remarketing_create(adwords_link = adword_link,
#'                      include = my_remarketing1, 
#'                      exclude = my_remarketing2,
#'                      audienceType = "STATE_BASED", 
#'                      name = "my_remarketing_seg1")

#' 
#' }
ga_remarketing_create <- function(adwordsLinkId,
                                  include,
                                  exclude = NULL,
                                  audienceType = c("SIMPLE","STATE_BASED"),
                                  name = NULL){
  audienceType <- match.arg(audienceType)
  
  assert_that(
    length(as.character(adwordsLinkId)) == 1,
    is.ga4_remarketing_segment(include)
  )
  
  assert_that_ifnn(exclude, is.ga4_remarketing_segment)
  assert_that_ifnn(name, is.string)
  
  if(audienceType == "SIMPLE"){
    # we only need include
    ra <- list(
      audienceType = "SIMPLE",
      audienceDefinition = list(
        includeConditions = list(
          segment = include[["conditions"]],
          membership = include[["membershipDurationDays"]],
          daysToLookBack = include[["daysToLookBack"]]
        )
      )
    )
  } else if(audienceType == "STATE_BASED"){
    ra <- list(
      audienceType = "STATE_BASED",
      stateBasedAudienceDefinition = list(
        includeConditions = list(
          segment = include[["conditions"]],
          membership = include[["membershipDurationDays"]],
          daysToLookBack = include[["daysToLookBack"]]
        ),
        excludeConditions = list(
          segment = exclude[["conditions"]],
          membership = exclude[["membershipDurationDays"]],
          daysToLookBack = exclude[["daysToLookBack"]]
        )
      )
    )
  } else {
    stop("Unknown audienceType:", audienceType, call. = FALSE)
  }
  
  ra <- rmNullObs(ra)
  
  ra
  
  # ga_remarketing_do_call(ra, accountId, webPropertyId)
  
}

#' Creates a new remarketing audience (Internal API call).
#' 
#' @seealso \href{https://developers.google.com/analytics/}{Google Documentation}
#' 
#' @details 
#' Authentication scopes used by this function are:
#' \itemize{
#'   \item https://www.googleapis.com/auth/analytics.edit
#' }
#' 
#' 
#' @param RemarketingAudience The \link{RemarketingAudience} object to pass to this method
#' @param accountId The account ID for which to create the remarketing audience
#' @param webPropertyId Web property ID for which to create the remarketing audience
#' @importFrom googleAuthR gar_api_generator
#' @family RemarketingAudience functions
#' @keywords internal
#' @noRd
ga_remarketing_do_call <- function(RemarketingAudience, 
                                  accountId, 
                                  webPropertyId) {
  url <- sprintf("https://www.googleapis.com/analytics/v3/management/accounts/%s/webproperties/%s/remarketingAudiences", 
                 accountId, webPropertyId)
  # analytics.management.remarketingAudience.insert
  f <- gar_api_generator(url, "POST", data_parse_function = function(x) x)
  stopifnot(inherits(RemarketingAudience, "gar_RemarketingAudience"))
  
  f(the_body = RemarketingAudience)
  
}

#' RemarketingAudience Object
#' 
#' @details 
#' Autogenerated via \code{\link[googleAuthR]{gar_create_api_objects}}
#' JSON template for an Analytics remarketing audience.
#' 
#' @param RemarketingAudience.audienceDefinition The \link{RemarketingAudience.audienceDefinition} object or list of objects
#' @param RemarketingAudience.stateBasedAudienceDefinition The \link{RemarketingAudience.stateBasedAudienceDefinition} object or list of objects
#' @param RemarketingAudience.stateBasedAudienceDefinition.excludeConditions The \link{RemarketingAudience.stateBasedAudienceDefinition.excludeConditions} object or list of objects
#' @param accountId Account ID to which this remarketing audience belongs
#' @param audienceDefinition The simple audience definition that will cause a user to be added to an audience
#' @param audienceType The type of audience, either SIMPLE or STATE_BASED
#' @param id Remarketing Audience ID
#' @param linkedAdAccounts The linked ad accounts associated with this remarketing audience
#' @param linkedViews The views (profiles) that this remarketing audience is linked to
#' @param name The name of this remarketing audience
#' @param stateBasedAudienceDefinition A state based audience definition that will cause a user to be added or removed from an audience
#' @param webPropertyId Web property ID of the form UA-XXXXX-YY to which this remarketing audience belongs
#' 
#' @return RemarketingAudience object
#' 
#' @family RemarketingAudience functions
#' @noRd
RemarketingAudience <- function(RemarketingAudience.audienceDefinition = NULL, 
                                RemarketingAudience.stateBasedAudienceDefinition = NULL, 
                                RemarketingAudience.stateBasedAudienceDefinition.excludeConditions = NULL, 
                                accountId = NULL, 
                                audienceDefinition = NULL, 
                                audienceType = NULL, 
                                id = NULL, 
                                linkedAdAccounts = NULL, 
                                linkedViews = NULL, 
                                name = NULL, 
                                stateBasedAudienceDefinition = NULL, 
                                webPropertyId = NULL) {
  structure(list(RemarketingAudience.audienceDefinition = RemarketingAudience.audienceDefinition, 
                 RemarketingAudience.stateBasedAudienceDefinition = RemarketingAudience.stateBasedAudienceDefinition, 
                 RemarketingAudience.stateBasedAudienceDefinition.excludeConditions = RemarketingAudience.stateBasedAudienceDefinition.excludeConditions, 
                 accountId = accountId, audienceDefinition = audienceDefinition, audienceType = audienceType, 
                 id = id,  linkedAdAccounts = linkedAdAccounts, 
                 linkedViews = linkedViews, name = name, stateBasedAudienceDefinition = stateBasedAudienceDefinition, 
                 webPropertyId = webPropertyId), class = "gar_RemarketingAudience")
}