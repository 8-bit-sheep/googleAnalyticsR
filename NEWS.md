# 0.2.1.9000

* add bigQuery asynch to Google Analytics 360 exports, for over 1,000,000 rows.
* A GA v4 example Shiny app added that runs on `https://mark.shinyapps.io/googleAnalyticsRv4Demo/`
* Fix bug for calculated metrics if expression not starting with `ga:` (#28)
* Add several management functions: accounts, webproperties, goals, adwords, custom data sources, custom metrics and dimensions etc.

# 0.2.1 CRAN

* Fix v4 bug where batches over 10000 had extra rows added (#19)

# 0.2.0

* segment helper as RStudio Addin or `gadget_GASegment()`
* Fix order_type() for v4 (#18)
* Local tests added
* Multi-account batching for v3 (#17)
* Add BigQuery fetching for GoogleAnalytics 360 accounts via `google_analytics_bq()`

# 0.1.1

* Patch to fix ignoring first row of v4 API results (#16)

# 0.1.0

* v4 API calls implemented
