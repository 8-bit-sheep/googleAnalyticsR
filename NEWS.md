# 0.2.1.9000

* Add BigQuery asynch fetch to Google Analytics 360 exports, for over 1,000,000 rows.
* A GA v4 example Shiny app added that runs on `https://mark.shinyapps.io/googleAnalyticsRv4Demo/`
* Fix bug for calculated metrics if expression not starting with `ga:` (#28)
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
* Fix a parse bug if only metrics and no dimensions were used in `google_analytics_4()`
* Refactoring v4 batching
* New argument `anti_sample` for `google_analytics_4()` which will split up the calls into efficient batches to avoid sampling. Includes experimental sub-day hourly batching.
* Set argument `max = -1` in `google_analytics_4()` to fetch all results. 
* Add fix for `google_analytics_account_summary` parsing when no views in web properties.
* New argument `anti_sample_batches` lets you choose batch sizes (daily is equal to `anti_sample_batches=1`)

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
