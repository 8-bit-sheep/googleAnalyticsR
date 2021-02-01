# googleAnalyticsR - Google Analytics API to R

![CRAN](http://www.r-pkg.org/badges/version/googleAnalyticsR)
[![codecov](https://codecov.io/gh/MarkEdmondson1234/googleAnalyticsR/branch/master/graph/badge.svg)](https://codecov.io/gh/MarkEdmondson1234/googleAnalyticsR)
[![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/2025/badge)](https://bestpractices.coreinfrastructure.org/projects/2025)
![CloudBuild](https://badger-ewjogewawq-ew.a.run.app/build/status?project=mark-edmondson-gde&id=4ae2fa13-b1d8-41f3-b846-8bf3c67f050a)
[![CodeFactor](https://www.codefactor.io/repository/github/markedmondson1234/googleanalyticsr/badge)](https://www.codefactor.io/repository/github/markedmondson1234/googleanalyticsr)


## googleAnalyticsR

Welcome to the website for **googleAnalyticsR**, an R library for working with [Google Analytics](https://developers.google.com/analytics/) data.

![](https://raw.githubusercontent.com/MarkEdmondson1234/googleAnalyticsR/master/inst/hexlogo/hex.png)

Follow development on the project's [Github development site.](https://github.com/MarkEdmondson1234/googleAnalyticsR)

The Slack group `googleAuthRverse` includes a `#googleAnalyticsR` channel. For news, chat and support join via this [request form](https://docs.google.com/forms/d/e/1FAIpQLSerjirmMpB3b7LmBs_Vx_XPIE9IrhpCpPg1jUcpfBcivA3uBw/viewform). 

Collaboration is welcomed and encouraged, if you are interested get in touch.

## Features

* First [Google Analytics Reporting v4 API](https://code.markedmondson.me/googleAnalyticsR/articles/v4.html) library for R
* Automatic [anti-sampling techniques](https://code.markedmondson.me/googleAnalyticsR/articles/v4.html#anti-sampling) to return more detailed data
* v4 features include: metric expressions, pivots, date comparisons, batching.
* Auto-paging, [auto-authentication](https://code.markedmondson.me/googleAnalyticsR/articles/setup.html) options
* API metadata of possible metrics and dimensions
* GA360 support for features such as [resource quotas](https://code.markedmondson.me/googleAnalyticsR/articles/v4.html#ga360_quota_system).
* Multi-user login to enable [Google Analytics powered Shiny Apps](https://code.markedmondson.me/googleAnalyticsR/articles/shiny.html)
* Integration with [BigQuery Google Analytics Premium/360 exports](https://code.markedmondson.me/googleAnalyticsR/articles/big-query.html).
* Single authentication flow can be used with other [`googleAuthR`](https://code.markedmondson.me/googleAuthR/) apps like [`searchConsoleR`](https://github.com/MarkEdmondson1234/searchConsoleR)
* Automatic batching, multi-account fetching, multi-channel funnnels
* Support for `googleAuthR` batch.  For big data calls this could be 10x quicker than normal GA fetching
* Meta data in attributes of returned dataframe including date ranges, totals, min and max
* The `.gamr` file format for [sharing modelling analysis of Google Analytics data](https://code.markedmondson.me/googleAnalyticsR/articles/models.html).
* Automatic caching of API requests
* Management API features such as [unsampled report downloads](https://code.markedmondson.me/googleAnalyticsR/articles/management.html#unsampled_reports) and [view filter setup](https://code.markedmondson.me/googleAnalyticsR/articles/management.html#view_filters)

## Thanks 

These are all great libraries that also work with Google Analytics data in R, that I have taken inspiration from.

* [rga](https://github.com/skardhamar/rga)
* [RGA](https://bitbucket.org/unikum/rga)
* [RGoogleAnalytics](https://github.com/Tatvic/RGoogleAnalytics)
* [ganalytics](https://github.com/jdeboer/ganalytics)
* [GAR](https://github.com/andrewgeisler/GAR)

Thanks also to the many contributors to the package that have been creating features, finding and fixing bugs and giving feedback.  They have all helped create the package as it is today.  In particular Jas Sohi (@j450h1) and Zoran Selinger (@zselinger) for their work on new management API features.

## Tutorials

If you have used the package and published online then get in touch and I'll list it here so others can benefit.

* [Dartistics.com](http://www.dartistics.com/) is a website dedicated to using R with digital analytics, and has a page on getting up and running with [googleAnalyticsR](http://www.dartistics.com/api-google-analytics.html)
* [Video tutorials for googleAnalyticsR](https://www.youtube.com/playlist?list=PLAMHKI_J4xv0esgbTYCnNuwQO0z3zrc6K) are on YouTube. 
* Michal Brys has an online book on using [Google Analytics with R](https://michalbrys.gitbooks.io/r-google-analytics/content/) that uses googleAnalyticsR
* Ryan Prazki has created a video tutorial on how to get up and running quickly: [Google Analytics R Video Tutorial](http://www.ryanpraski.com/google-analytics-r-tutorial/)
* Ryan also has a tutorial on using googleAnalyticsR to do [scroll tracking analysis in a heatmap](http://www.ryanpraski.com/scroll-depth-tracking-analysis-with-google-analytics-r)
* Jules has a nice post on using a [Markov model to create an attribution model with googleAnalyticsR data](http://stuifbergen.com/2016/11/conversion-attribution-markov-model-r/)
* Tim Wilson has a guide on getting up and running with [R for digital analysts](http://analyticsdemystified.com/google-analytics/tutorial_pulling_google_analytics_data_with_r/)
* Guide to using [googleAnalyticsR in Japanese](https://www.karada-good.net/analyticsr/r-520) and [here](http://abrahamcow.hatenablog.com/entry/2016/05/22/121721)
* [Alenlytics](http://alenlytics.com/connecting-r-with-google-analytics/) has a guide that also includes using the RStudio Addin for authentication
* Mario Martinez has a guide on using [googleAnalyticsR in Spanish](http://www.doctormetrics.com/2017/01/03/nueva-version-de-la-api-de-google-analytics-r-statistics/#.WLVOWhAvpBI)
* Vincent has a tutorial on using [Twitters AnomalyDetection package with googleAnalyticsR](https://data-seo.fr/2016/05/02/detecter-vos-marronniers-avec-r/) in French
* Tom Alby uses `googleAnalyticsR` to do some [clustering market basket analysis](https://tom.alby.de/clustering-mit-google-analytics-und-r/) in German.
* Anita makes some [pie and bar charts](https://rpubs.com/anitaowens/googleapi) from channel data 
* Creating [Google Analytics non-sampled scheduled reports](https://2steps.pro/google-anslytics-data-without-sampling-scheduler.html) [Russian]
* Fredrik [forecasts sales from Google Analytics and sends it to your own Slack bot](https://www.linkedin.com/pulse/prognostisera-data-fr%C3%A5n-google-analytics-med-r-fredrik-cederl%C3%B6f/) [Swedish]
* Fredrik [analyses time between bookings and events](https://medium.com/@fredrikcederlof/analyzing-time-between-bookings-and-events-with-r-google-analytics-293f6c334d73) with lots of detail on interpreting the data.
* Becky West at Lunametrics shows how to use some of the management API functions to [audit your GA configurations](https://www.lunametrics.com/blog/2018/03/08/google-analytics-check-r-management-api/)
* Pierre has a [guide in French](https://www.anakeyn.com/2018/03/01/googleanalyticsr-import-google-analytics-r/) on how to setup googleAnalyticsR and start pulling out data.
* Nico has a guide on how to insert [Google Analytics data into a map](https://www.ridgeway.com/blog/performance-optimisation/how-to-insert-ga-data-into-a-map-using-r-and-build)
* Dusan goes into how to setup and use [googleAnalyticsR in Serbian](https://dusanmilosevic.com/google-analytics-api-v4-i-r-programski-jezik/)
* Pavel talks about how to [automate website analytics with R and googleAnalyticsR](https://fathomfuel.com/automating-website-analytics-with-r-part-1/)
* Mayo shows how to [visualise geographical data from your Google Analytics data](https://www.mayoracek.com/r-programming/visualize-geographical-data-help-google-analytics-r/)
* Habr.com show how to get data from [Google Analytics into Microsoft SQL Server](https://habr.com/en/post/466589/) via googleAnalyticsR
* Ruben has a [beginners guide to googleAnalyticsR](https://www.rubenvezzoli.online/googleanalyticsr-beginners-guide/)
* Adam Ribaudo has some nice resources on analysis with `googleAnalyticsR` including posts on [market basket analysis (MBA) with Google Analytics data](https://www.noisetosignal.io/2020/05/market-basket-analysis-using-google-analytics-data/) and [viewing Google Analytics segment overlap](https://www.noisetosignal.io/2020/05/viewing-google-analytics-segment-overlap-in-r/)
* Chloe writes on how to use googleAnalyticsR to [detect spam traffic in your Google Analytics data](https://medium.com/@the.numerist/detecting-spam-traffic-in-google-analytics-data-with-googleanalyticsr-d2a97b83e926)
* Carl from RStudio writes about the process of [creating custom Google Analytics dashboards with R](https://blog.rstudio.com/2020/11/27/google-analytics-part1/).
* Antoine writes a nice in-depth post on [analysis you can do with your blog traffic in R](https://www.statsandr.com/blog/track-blog-performance-in-r/)

## My website

There are some blog posts on using googleAnalyticsR and various other data science code for digital marketing on my blog: [Mark Edmondson's Blog](https://code.markedmondson.me).  

There you can also see other [R packages working with digital marketing applications](http://code.markedmondson.me/r-packages/) such as `searchConsoleR`, `bigQueryR` and `googleMeasureR`.

