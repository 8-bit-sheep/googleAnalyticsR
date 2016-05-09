## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.3.0
* win-builder (devel and release)

## 2 Notes

Possibly mis-spelled words in DESCRIPTION:
  API (4:25, 6:13)
  Analytics (4:15, 5:56)
  
These words are spelt correctly.

Found the following (possibly) invalid URLs:
  URL: https://console.developers.google.com/apis/library
    From: inst/doc/googleAnalyticsR.html
          README.md
    Status: 404
    Message: Not Found

This is the correct URL, but it lies behing a login that the CRAN checker can't use. 