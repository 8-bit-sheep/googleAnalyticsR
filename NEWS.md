# googleAnalyticsR 0.3.0.9000

## Added

* Add custom data uploads via `ga_custom_upload_file()`
* Cleaned up documentation a bit
* Added auto-authentication if you specify environment var `GA_AUTH_FILE`

## Fixed 

* Fix bug with anti-sample when periods included 0-row data (#42)

# googleAnalyticsR  0.3.0

## Added

* Add BigQuery asynch fetch to Google Analytics 360 exports, for over 1,000,000 rows.
* A GA v4 example Shiny app added that runs on `https://mark.shinyapps.io/googleAnalyticsRv4Demo/`
* New argument `anti_sample` for `google_analytics_4()` which will split up the calls into efficient batches to avoid sampling. Includes experimental sub-day hourly batching.
* Set argument `max = -1` in `google_analytics_4()` to fetch all results. 
* Add fix for `google_analytics_account_summary` parsing when no views in web properties.
* New argument `anti_sample_batches` lets you choose batch sizes (daily is equal to `anti_sample_batches=1`)
* Add several management API functions:
    - `ga_accounts`
    - `ga_adwords`
    - `ga_adwords_list`
    - `ga_custom_datasource`
    - `ga_custom_upload`
    - `ga_custom_upload_list`
    - `ga_custom_vars`
    - `ga_custom_vars_list`
    - `ga_experiment`
    - `ga_experiment_list`
    - `ga_filter`
    - `ga_filter_list`
    - `ga_filter_view`
    - `ga_filter_view_list`
    - `ga_goal`
    - `ga_goal_list`
    - `ga_segment_list`
    - `ga_unsampled`
    - `ga_unsampled_list`
    - `ga_view`
    - `ga_view_list`
    - `ga_webproperty`
    - `ga_webproperty_list`

## Fixed

* Fix bug for calculated metrics if expression not starting with `ga:` (#28)
* Fix a parse bug if only metrics and no dimensions were used in `google_analytics_4()`
* Refactoring v4 batching


# googleAnalyticsR 0.2.1 

## Fixed

* Fix v4 bug where batches over 10000 had extra rows added (#19)

# googleAnalyticsR 0.2.0

## Added

* segment helper as RStudio Addin or `gadget_GASegment()`
* Local tests added
* Multi-account batching for v3 (#17)
* Add BigQuery fetching for GoogleAnalytics 360 accounts via `google_analytics_bq()`

## Fixed

* Fix order_type() for v4 (#18)

# googleAnalyticsR  0.1.1

## Fixed

* Patch to fix ignoring first row of v4 API results (#16)

# googleAnalyticsR 0.1.0

## Added

* v4 API calls implemented
