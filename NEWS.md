# googleAnalyticsR 0.7.0

* Clean up error messages
* Fix anti_sampling breaking if using date formats "4daysAgo" etc.
* Removed `google_analytics_meta()` for `ga_meta()`
* Add `accountId` to the output of Shiny module `authDropdown` when `viewIdOnly=FALSE`
* Add `quotaUser` to reporting v4 API requests to avoid quota limits if several users from same ip attempt to run API calls (#235) - default is `Sys.info()[["user"]]` or set via `options("googleAuthR.quotaUser")`
* Add `ga_model_*` functions to use GA data (#234)
* Add `ga_model_tweet` to enable data viz in tweets
* Add userActivity API via `gar_clientid_activity()`
* Use new authentication setup via `googleAuthR v1.0.0` and `gargle`
* Prevent use of default client id project in non-interactive scripts.  
* Update docs to use `clientId` dimension in user activity API calls
* Update `tidyr::unnest` to use `cols` parameter (#266)
* Add `ga_custom_upload_delete()` - thanks @byapprov (#263)
* Fix sending in multiple segment definitions in a list (#253)
* Fix `ga_segment_list()` returning data if no custom segments created (#259)
* Fix ability to list multiple dimensions and segments (#279)

# googleAnalyticsR 0.6.0

* Add goal management
* Add `ga_clientid_deletion()` function (#168) (renamed from `ga_user_deletion()`)
* Let `authDropdown` return more than just the viewId (#172)
* Export segment related classes and implicitly coerce table filters and segments to their appropriate classes in order to improve integration with the ganalytics package.
* Add support for `dataLastRefreshed` and stop caching if its present (e.g. query includes today) (#183)
* Export dim_ga4, met_ga4 and order_type_ga4 classes in order to support the complete conversion of `ganalytics` query objects into googleAnalyticsR API requests.
* Fix segments that were not respecting "IMMEDIATELY_PRECEDES" vs "PRECEDES" (thanks @digitos) (#180)
* Fix default of pivot groups so they include first column (thanks @gilliganondata) (#107)
* Support for creating remarketing audiences (#161)
* Support "NdaysAgo" in antisample calls (#209)
* Support more than 1000 entries in various management list functions (#174)
* Parse management API list responses into data.frames (#194)
* Add user management functions such as `ga_users_delete`, `ga_users_update` and `ga_users_add`
* Add custom dimension functions `ga_custom_vars_create` and `ga_custom_vars_patch`

# googleAnalytics 0.5.0

## Breaking changes!

If you were using `google_analytics()` before to fetch the v3 API, this is now available via `google_analytics_3()` - replace all instances and it should work as before.  However, you are encouraged to migrate to v4, which now runs when you use `google_analytics()`(and for a while still at `google_analytics_4()` too)

## Major changes

* Add support for resource based quotas if you are on GA360 (#127)
* Improve support for using different auth cache files with `ga_auth()`
* Changed `google_analytics` to be the v4 API, `google_analytics_3` now supports v3
* change default cache token name from `.httr-oauth` to `ga.oauth` to avoid clashes
* You can now change the rows fetched per API page in v4 up to 100k
* Add unsampled report downloads (#44 - many thanks to @j450h1 for this work on this) 
* Add management of View and account filters (#108 - many thanks to @zselinger for the work on this)
* If an `google_analytics` batch API call fails, it will automatically retry with a slower request rate
* v4 API requests will report how long it took to help with configurations

## Bug fixes

* Fix bug where anti-sampling with no dimensions broke (#149 - thanks @smach)
* Let v3 API calls use batching when also using other googleAuthR batching functions

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
