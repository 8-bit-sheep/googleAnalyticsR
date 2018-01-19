options(googleAuthR.client_id = Sys.getenv("GA_CLIENT_ID"))
options(googleAuthR.client_secret = Sys.getenv("GA_CLIENT_SECRET"))
library(googleAuthR)
gar_auth(token = ".httr-oauth")

library(devtools)
devtools::load_all()

## this one is ok
google_analytics_bq("blah","foo", start = "2017-01-01", end = "2017-02-01", metrics = "sessions", dimensions = "source", return_query_only = TRUE)
#[1] "SELECT trafficSource.source as source, SUM(totals.visits) as sessions FROM (TABLE_DATE_RANGE([foo.ga_sessions_], TIMESTAMP('2017-01-01'), TIMESTAMP('2017-02-01'))) GROUP BY source  LIMIT 100"

## invalid dimension
google_analytics_bq("blah","foo", start = "2017-01-01", end = "2017-02-01", metrics = "sessions", dimensions = "adWordsQuery", return_query_only = TRUE)

## invalid metric
google_analytics_bq("blah","foo", start = "2017-01-01", end = "2017-02-01", metrics = "sales", dimensions = "source", return_query_only = TRUE)

# null metric
google_analytics_bq("blah","foo", start = "2017-01-01", end = "2017-02-01", dimensions = "source", return_query_only = TRUE)

# null dimension
google_analytics_bq("blah","foo", start = "2017-01-01", end = "2017-02-01", metrics = "sessions", return_query_only = TRUE)
