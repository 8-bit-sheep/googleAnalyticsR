# googleAnalytics 0.4.2

## Bug fixes

* Fix bug of argument match in `multi_select`(#99 - thanks @sdhaus)
* Fix bug for data dimensions starting with a number (#100 - thanks @kennyengci)
* Remove hourly anti-sampling as it doesn't work
* Use tidy eval instead of dplyr underscore funcs (thanks @kiendang)
* Fix multi-channel funnels raising error when no data available (thanks @octaviancorlade)
* Fix NULL validaation if no results returns in v4 API (#81 again - thanks @ricardopinto)

## Additions

* You can now set your `GA_CLIENT_ID`, `GA_CLIENT_SECRET` in environment arguments and they will be added to the project options on startup. 
* Authenticate on startup with `GA_AUTH_FILE` pointing to your cache file.
* Add support for "today" and "yesterday" date values in anti_sample (#112)
* Add that `max` is ignored during `anti_sample=TRUE` (#111)

# googleAnalyticsR 0.4.1

## Fixed

* Fix `attempt to set an attribute on NULL` error for anti-sample
* Fixed `anti_sample="auto"` math to actually limit the number of sessions for the auto-batches
* Fixed error when no data present in requested `date_range` for a view, and `max = -1`
* Fix logic of `anti_sample_batches` to allow accurate day batches. (#74)
* Fix parsing of `ga_account_summary` broken by upgrade from `dplyr 0.5.0` > `dplyr 0.7.0`

## Added

* Add warning if using default project, and if API quota is tripped (#79)
* refactor defensive checks from `testthat` to `assertthat`

# googleAnalyticsR 0.4.0

## Added

* Deprecate `google_analytics_account_list` for `ga_account_list` to be in line with other functions
* Add custom data uploads via `ga_custom_upload_file()`
* Cleaned up documentation a bit
* Added auto-authentication if you specify environment var `GA_AUTH_FILE`
* Add Remarketing Audience functions - `ga_remarketing_get` and `ga_remarketing_list`
* Add `aggregateGAData` which will aggregate over GA dimensions
* Add `antiSampleWorked` TRUE/FALSE attribute to tables when anti-sampling attempted
* Add a `slow_fetch` flag to `google_analytics_4` that will avoid batching for big complicated fetches
* Add `ga_users_list` for listing users on account, webProperty or View level
* Set default Google Project API permissions to:
    - `"https://www.googleapis.com/auth/analytics"`
    - `"https://www.googleapis.com/auth/analytics.readonly"`
    - `"https://www.googleapis.com/auth/analytics.manage.users.readonly"`
    - `"https://www.googleapis.com/auth/analytics.edit"`
    - `"https://www.googleapis.com/auth/analytics.manage.users"`

## Fixed 

* Use `aggregateGAData` to solve issue with anti_sampling repeating rows (#49)
* Fix bug with anti-sample when periods included 0-row data (#42)
* Fix `google_analytics_account_list()` parsing bug if only one view available (#52, #41)
* Fix segments not being able to be used in Cohort reports (#63)
* Fix bug with anti-sample when querying over 1000 days (#66 - thanks @olivia-icivics)

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
