![](vignettes/website/gar_logo_1000.png)

Welcome to the website for **googleAnalyticsR**, an R library for working with Google Analytics data.

Follow development on the project's [Github development site.](https://github.com/MarkEdmondson1234/googleAnalyticsR)

Any bugs or feature requests please [file an issue.](https://github.com/MarkEdmondson1234/googleAnalyticsR/issues)

Collaboration is welcomed and encouraged, if you are interested get in touch.

[![Downloads](http://cranlogs.r-pkg.org/badges/googleAnalyticsR)](http://www.r-pkg.org/pkg/googleAnalyticsR)
![CRAN](http://www.r-pkg.org/badges/version/googleAnalyticsR)
[![Travis-CI Build Status](https://travis-ci.org/MarkEdmondson1234/googleAnalyticsR.svg?branch=master)](https://travis-ci.org/MarkEdmondson1234/googleAnalyticsR)

## Features

* First Google Analytics Reporting v4 API library for R
* v4 features include: dynamic calculated metrics, pivots, histograms, date comparisons, batching.
* v4 API explorer
* API metadata of possible metrics and dimensions
* Multi-user login in Shiny App
* Integration with BigQuery Google Analytics Premium/360 exports.
* Single authentication flow can be used with other [`googleAuthR`](http://code.markedmondson.me/googleAuthR/) apps like [`searchConsoleR`](https://github.com/MarkEdmondson1234/searchConsoleR)
* Automatic batching, sampling avoidance with daily walk, multi-account fetching, multi-channel funnnel
* Support for `googleAuthR` batch.  For big data calls this could be 10x quicker than normal GA fetching.
* Meta data in attributes of returned dataframe including date ranges, totals, min and max

## Thanks 

These are all great libraries that also work with Google Analytics data in R, that I have taken inspiration from.

* [rga](https://github.com/skardhamar/rga)
* [RGA](https://bitbucket.org/unikum/rga)
* [RGoogleAnalytics](https://github.com/Tatvic/RGoogleAnalytics)
* [ganalytics](https://github.com/jdeboer/ganalytics)
* [GAR](https://github.com/andrewgeisler/GAR)


