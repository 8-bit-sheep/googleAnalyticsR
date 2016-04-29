# googleAnalyticsR
[![Travis-CI Build Status](https://travis-ci.org/MarkEdmondson1234/googleAnalyticsR.svg?branch=master)](https://travis-ci.org/MarkEdmondson1234/googleAnalyticsR)

A new Google Analytics R library using the new v4 of the Google Analytics Reporting API.  Built using [`googleAuthR`](https://github.com/MarkEdmondson1234/googleAuthR). The successor to [`shinyga`](https://github.com/MarkEdmondson1234/shinyga) it allows online OAuth2 authentication within Shiny apps, along with new features such as batching and compatibility with other Google APIs.

Work is currently focused on getting feature parity with `shinyga` such as the GA Management API features. 

Please log any problems here in the issues tracker.

## Other GA R libraries

These are all great libraries, that I have taken inspiration from.

* [rga](https://github.com/skardhamar/rga)
* [RGA](https://bitbucket.org/unikum/rga)
* [RGoogleAnalytics](https://github.com/Tatvic/RGoogleAnalytics)
* [ganalytics](https://github.com/jdeboer/ganalytics)
* [GAR](https://github.com/andrewgeisler/GAR)

## Why do we need another GA library?

* First Google Analytics Reporting v4 API library for R
* v4 features include: dynamic calculated metrics, pivots, histograms, date comparisons, batching.
* Shiny App compatible
* The same authentication flow can be used with other `googleAuthR` apps like [`searchConsoleR`](https://github.com/MarkEdmondson1234/searchConsoleR)
* Automatic batching, sampling avoidance with daily walk, multi-account fetching, multi-channel funnnel
* Support for `googleAuthR` batch, meaning 10 calls at once to GA - great for big fetches.  For big data calls this is implemented automatically so could be 10x quicker than normal GA fetching
* Meta data included in attributes of returned dataframe including date ranges, totals, min and max

## Install

Development version - needs version > `0.2.0.9000` of `googleAuthR` too until the latest changes are on CRAN

```
devtools::install_github("MarkEdmondson1234/googleAuthR")
devtools::install_github("MarkEdmondson1234/googleAnalyticsR")
```

## To use - v4 API calls

Todo.  But check out `?google_analytics_4` and these example queries:

```r
library(googleAuthR)
library(googleAnalyticsR)

## authenticate, or use the RStudio Addin "Google API Auth" with analytics scopes set
ga_auth()

## get your accounts
account_list <- google_analytics_account_list()

## pick a profile with data to query
ga_id <- account_list[31,'viewId']

## create filters on metrics
mf <- met_filter("bounces", "GREATER_THAN", 0)
mf2 <- met_filter("sessions", "GREATER", 2)

## create filters on dimensions
df <- dim_filter("source","BEGINS_WITH","1",not = TRUE)
df2 <- dim_filter("source","BEGINS_WITH","a",not = TRUE)

## construct filter objects
fc2 <- filter_clause_ga4(list(df, df2), operator = "AND")
fc <- filter_clause_ga4(list(mf, mf2), operator = "AND")

## make v4 request
## demo showing how the new filters work
ga_data1 <- google_analytics_4(ga_id, 
                              date_range = c("2015-07-30","2015-10-01"),
                              dimensions=c('source','medium'), 
                              metrics = c('sessions','bounces'), 
                              met_filters = fc, 
                              dim_filters = fc2, 
                              filtersExpression = "ga:source!=(direct)")

> str(ga_data1)
'data.frame':	11 obs. of  6 variables:
 $ source     : chr  "baby.dk" "bing" "buttons-for-website.com" "duckduckgo.com" ...
 $ medium     : chr  "referral" "organic" "referral" "referral" ...
 $ sessions.d1: num  3 71 7 5 642 3 3 35 11 66 ...
 $ bounces.d1 : num  2 42 7 3 520 2 1 35 11 43 ...
 $ sessions.d2: num  6 217 0 0 1286 ...
 $ bounces.d2 : num  3 126 0 0 920 9 0 0 0 178 ...
 - attr(*, "totals")=List of 2
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "852"
  .. .. ..$ : chr "670"
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "1766"
  .. .. ..$ : chr "1240"
 - attr(*, "minimums")=List of 2
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "3"
  .. .. ..$ : chr "1"
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "0"
  .. .. ..$ : chr "0"
 - attr(*, "maximums")=List of 2
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "642"
  .. .. ..$ : chr "520"
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "1286"
  .. .. ..$ : chr "920"
 - attr(*, "isDataGolden")= logi TRUE
 - attr(*, "rowCount")= int 11
              
## demo of querying two date ranges at a time   
## we make the request via make_ga_4_req to use in next demo
multidate_test <- make_ga_4_req(ga_id, 
                                date_range = c("2015-07-30",
                                               "2015-10-01",
                                                "2014-07-30",
                                                "2014-10-01"),
                                dimensions = c('source','medium'), 
                                metrics = c('sessions','bounces'), 
                                met_filters = fc, 
                                dim_filters = fc2, 
                                filtersExpression = "ga:source!=(direct)")
                                
ga_data2 <- fetch_google_analytics_4(list(multidate_test))

## Demo querying two reports at the same time
## Use make_ga_4_req() to make multiple requests and then send 
##   them as a list to fetch_google_analytics_r()
multidate_test2 <- make_ga_4_req(ga_id,
                                 date_range = c("2015-07-30",
                                                "2015-10-01",
                                                "2014-07-30",
                                                "2014-10-01"),
                                 dimensions=c('hour','medium'), 
                                 metrics = c('visitors','bounces'), 
                                 met_filters = fc, 
                                 dim_filters = fc2, 
                                 filtersExpression = "ga:source!=(direct)")

ga_data3 <- fetch_google_analytics_4(list(multidate_test, multidate_test2)) 

> str(ga_data3)
List of 2
 $ :'data.frame':	11 obs. of  6 variables:
  ..$ source     : chr [1:11] "baby.dk" "bing" "buttons-for-website.com" "duckduckgo.com" ...
  ..$ medium     : chr [1:11] "referral" "organic" "referral" "referral" ...
  ..$ sessions.d1: num [1:11] 3 71 7 5 642 3 3 35 11 66 ...
  ..$ bounces.d1 : num [1:11] 2 42 7 3 520 2 1 35 11 43 ...
  ..$ sessions.d2: num [1:11] 6 217 0 0 1286 ...
  ..$ bounces.d2 : num [1:11] 3 126 0 0 920 9 0 0 0 178 ...
  ..- attr(*, "totals")=List of 2
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "852"
  .. .. .. ..$ : chr "670"
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "1766"
  .. .. .. ..$ : chr "1240"
  ..- attr(*, "minimums")=List of 2
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "3"
  .. .. .. ..$ : chr "1"
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "0"
  .. .. .. ..$ : chr "0"
  ..- attr(*, "maximums")=List of 2
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "642"
  .. .. .. ..$ : chr "520"
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "1286"
  .. .. .. ..$ : chr "920"
  ..- attr(*, "isDataGolden")= logi TRUE
  ..- attr(*, "rowCount")= int 11
 $ :'data.frame':	36 obs. of  6 variables:
  ..$ hour       : chr [1:36] "00" "00" "01" "02" ...
  ..$ medium     : chr [1:36] "organic" "referral" "organic" "organic" ...
  ..$ visitors.d1: num [1:36] 28 3 43 53 3 39 5 43 42 45 ...
  ..$ bounces.d1 : num [1:36] 16 2 28 41 3 27 5 34 30 34 ...
  ..$ visitors.d2: num [1:36] 85 1 93 94 5 87 6 103 83 81 ...
  ..$ bounces.d2 : num [1:36] 59 1 66 67 4 61 5 73 60 50 ...
  ..- attr(*, "totals")=List of 2
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "838"
  .. .. .. ..$ : chr "661"
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "1844"
  .. .. .. ..$ : chr "1303"
  ..- attr(*, "minimums")=List of 2
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "3"
  .. .. .. ..$ : chr "2"
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "1"
  .. .. .. ..$ : chr "1"
  ..- attr(*, "maximums")=List of 2
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "65"
  .. .. .. ..$ : chr "56"
  .. ..$ :List of 1
  .. .. ..$ values:List of 2
  .. .. .. ..$ : chr "142"
  .. .. .. ..$ : chr "101"
  ..- attr(*, "isDataGolden")= logi TRUE
  ..- attr(*, "rowCount")= int 36
  
## demo showing on-the-fly calculated metrics
ga_data4 <- google_analytics_4(ga_id,
                               date_range = c("2015-07-30",
                                              "2015-10-01"),
                              dimensions=c('medium'), 
                              metrics = c(visitsPerVisitor = "ga:visits/ga:visitors",
                                          'bounces'), 
                              metricFormat = c("FLOAT","INTEGER"),
                              met_filters = fc, 
                              dim_filters = fc2, 
                              filtersExpression = "ga:source!=(direct)")
> str(ga_data4)
'data.frame':	2 obs. of  3 variables:
 $ medium          : chr  "organic" "referral"
 $ visitsPerVisitor: num  1.08 1.01
 $ bounces         : num  605 71
 - attr(*, "totals")=List of 1
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "1.0696517412935322"
  .. .. ..$ : chr "676"
 - attr(*, "minimums")=List of 1
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "1.0125"
  .. .. ..$ : chr "71"
 - attr(*, "maximums")=List of 1
  ..$ :List of 1
  .. ..$ values:List of 2
  .. .. ..$ : chr "1.0759668508287292"
  .. .. ..$ : chr "605"
 - attr(*, "isDataGolden")= logi TRUE
 - attr(*, "rowCount")= int 2
 - attr(*, "dates")=List of 1
  ..$ :List of 2
  .. ..$ startDate: chr "2015-07-30"
  .. ..$ endDate  : chr "2015-10-01"
```


## To use - v3 API calls

For syntax of filters and dimensions, this library parses in exactly as specified in the [Google Analytics v3 API docs](https://developers.google.com/analytics/devguides/reporting/core/v3/reference?hl=en#filters), so check those out.  Note you do not need to encode symbols, but may have to encode URLs if you are filtering for those in say ga:pagePath

```r
library(googleAuthR)
library(googleAnalyticsR)

## Authenticate in Google OAuth2
## this also sets options
ga_auth()

## if you need to re-authenticate use ga_auth(new_user=TRUE)

## if you have your own Google Dev console project keys, 
## then don't run ga_auth() as that will set the authentication project to the defaults.
## instead put your options here, and run googleAuthR::gar_auth()

## get account info, including View Ids
account_list <- google_analytics_account_list()

## get a list of what metrics and dimensions you can use
meta <- google_analytics_meta()
head(meta)

## pick the account_list$viewId you want to see data for.
## metrics and dimensions can have or have not "ga:" prefix
gadata <- google_analytics(id = account_list$viewId[1], 
                           start="2015-08-01", end="2015-08-02", 
                           metrics = c("sessions", "bounceRate"), 
                           dimensions = c("source", "medium"))

## multi accounts, pass character vector of viewIds
## outputs a list of data.frames, named after the viewId
multi_gadata <- google_analytics(id = c("123456","9876545","765432"), 
                                 start="2015-08-01", end="2015-08-02", 
                                 metrics = c("sessions", "bounceRate"), 
                                 dimensions = c("source", "medium"))

## if more than 10000 rows in results, auto batching
## example is setting lots of dimensions to try and create big sampled data
batch_gadata <- google_analytics(id = account_list$viewId[1], 
                                 start="2014-08-01", end="2015-08-02", 
                                 metrics = c("sessions", "bounceRate"), 
                                 dimensions = c("source", "medium", "landingPagePath","hour","minute"),
                                 max=99999999)

## mitigate sampling by setting samplingLevel="WALK"
## this will send lots and lots of calls to the Google API limits, beware
walk_gadata <- google_analytics(id = account_list$viewId[1], 
                                start="2014-08-01", end="2015-08-02", 
                                metrics = c("sessions", "bounceRate"), 
                                dimensions = c("source", "medium", "landingPagePath"), 
                                max=99999999, samplingLevel="WALK")

## multi-channel funnels set type="mcf"
mcf_gadata <- google_analytics(id = account_list$viewId[1], 
                               start="2015-08-01", end="2015-08-02", 
                               metrics = c("totalConversions"), 
                               dimensions = c("sourcePath"), 
                               type="mcf")

## google_analytics dataframes include these meta data attributes:
- attr(*, "containsSampledData")= logi FALSE
 - attr(*, "samplePercent")= num 100
 - attr(*, "samplingLevel")= chr "DEFAULT"
 - attr(*, "profileInfo")=List of 6
  ..$ profileId            : chr "535656"
  ..$ accountId            : chr "2748374"
  ..$ webPropertyId        : chr "UA-278374-1"
  ..$ internalWebPropertyId: chr "5279208"
  ..$ profileName          : chr "XXXXX"
  ..$ tableId              : chr "mcf:539656"
 - attr(*, "dateRange")=List of 2
  ..$ startDate: chr "2015-08-01"
  ..$ endDate  : chr "2015-08-02"
 - attr(*, "totalResults")= int 4

## reach meta-data via attr()
attr(gadata, "profileInfo")
attr(gadata, "dateRange")

```
## Using your own Google Developer Project API Key

With the amount of API calls possible with this library via batching and walking, its more likely the shared
Google API project will hit the 50,000 calls per day limit.

To mitigate that, use your own [Google Developer Console Project](https://console.developers.google.com/apis/library) key, so it is not shared across all users of this library.

Get your own OAuth2 clientID and client secret and add to the options like so:

TODO: guide on how you get your own client Id.

### Example adding your own Google Developer Console keys

Below a new client Id is set for both offline and Shiny use.  Put the lines at the top of your R script with your
own credentials.

In this example, the scopes are also set so a user can authenticate with searchConsoleR at the same time, which 
is possible as they both use googleAuthR as a backend.

```r
options("googleAuthR.client_id" = "994xxx55706-afliqtuxxxxxxx2fd4kesu6.apps.googleusercontent.com")
options("googleAuthR.client_secret" = "3JhLa_GxxxxxCQYLe31c64")
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics", 
                                          "https://www.googleapis.com/auth/analytics.readonly", 
                                          "https://www.googleapis.com/auth/webmasters"))
options("googleAnalyticsR.webapp.client_id" = "994xxxx55706-e9qompxxxxx9pcab1u005cv.apps.googleusercontent.com")
options("googleAnalyticsR.webapp.client_secret" = "zcofxxxxxxElemXN5sf")
```


## Shiny

To use in Shiny, use the googleAuth `with_shiny`

```r

## in server.R
library(googleAuthR)
library(googleAnalyticsR)
library(shiny)

shinyServer(function(input, output, session){
  
  ## Get auth code from return URL
  access_token  <- reactiveAccessToken(session)
  
  ## Make a loginButton to display using loginOutput
  output$loginButton <- renderLogin(session, access_token(),
                                    logout_class = "btn btn-danger")

  gadata <- reactive({

    gadata <- with_shiny(google_analytics,
                         id = "222222",
                         start="2015-08-01", end="2015-08-02", 
                         metrics = c("sessions", "bounceRate"), 
                         dimensions = c("source", "medium"),
                         shiny_access_token = access_token())

})
```
