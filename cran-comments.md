## Test environments
* local OS X install, R 3.3.0
* ubuntu 12.04 (on travis-ci), R 3.3.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 2 note

## 2 Notes

* Possibly mis-spelled words in DESCRIPTION:
  API (4:25, 6:13)
  Analytics (4:15, 5:56)
  
These words are spelt correctly.

* Found the following assignments to the global environment:
File ‘googleAnalyticsR/R/gadget.R’:
  assign(output_var, segment, envir = .GlobalEnv)

This is a RStudio Addin that creates an assignment only with user consent.

---



