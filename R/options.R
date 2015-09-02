.onLoad <- function(libname, pkgname) {

  op <- options()
  op.googleAnalyticsR <- list(
    googleAnalyticsR.client_id = "289759286325-97kjcr4m3f0089cfqcp9hbu9jl4b6bus.apps.googleusercontent.com",
    googleAnalyticsR.client_secret = "U2F718SsJvb3fxAMrFQLM-oZ",
    googleAnalyticsR.webapp.client_id = "289759286325-42j8nmkeq5n9v9eb1kiuj2i97v9oea1f.apps.googleusercontent.com",
    googleAnalyticsR.webapp.client_secret = "0zBtmZ_klTEzXUaTUTP5AkNQ",
    googleAnalyticsR.scope = c("https://www.googleapis.com/auth/analytics",
                               "https://www.googleapis.com/auth/analytics.edit"),
    googleAnalyticsR.tryAttempts = 5
  )
  toset <- !(names(op.googleAnalyticsR) %in% names(op))
  if(any(toset)) options(op.googleAnalyticsR[toset])

  invisible()

}
