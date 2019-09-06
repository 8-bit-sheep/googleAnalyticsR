## ----setup, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----
# auto-auth when done locally
library(googleAnalyticsR)

## ----account structure, message=FALSE, warning=FALSE---------------------
# ga_account_list is most commonly used
# (restricted to top 10 with the head() function)
head(ga_account_list(), n = 10)

# this only lists account meta-data
ga_accounts()

# this gives meta-data for all web-properties for this accountId
ga_webproperty_list(47480439)

# this is meta-data for one particular web-property
ga_webproperty(accountId = 47480439, webPropertyId = "UA-47480439-1")

# this is meta-data for the views under this accountId/webPropertyId
ga_view_list(accountId = 47480439, webPropertyId = "UA-47480439-1")

# this is meta-data for this particular viewId (profileId)
ga_view(accountId = 47480439, webPropertyId = "UA-47480439-1", profileId = 81416941)

## ------------------------------------------------------------------------
# you can just use `meta` as is to get the available metrics, or ensure an up to date version by calling the metadata API.
# here we just return the first 5 columns and rows for brevity
head(meta[,1:5])

head(ga_meta())[,1:5]

## ------------------------------------------------------------------------
# use `aggregateGAData` so you can on the fly create summary data
ga_data <- google_analytics(81416156, 
                            date_range = c("10daysAgo", "yesterday"),
                            metrics = c("sessions","bounceRate"), dimensions = c("hour","date"))

head(ga_data)

# if we want totals per hour over the dates:
ga_aggregate(ga_data[,c("hour","sessions")], agg_names = "hour")

# it knows not to sum metrics that are rates:
ga_aggregate(ga_data[,c("hour","bounceRate")], agg_names = "hour")

## ------------------------------------------------------------------------
#
amd <- ga_allowed_metric_dim()
head(amd)

