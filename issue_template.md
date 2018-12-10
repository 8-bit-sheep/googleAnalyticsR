## What goes wrong


## Steps to reproduce the problem



## Expected output



## Actual output

Before you run your code, please run:

`options(googleAuthR.verbose=2)` and copy-paste the console output here.  
Check it doesn't include any sensitive info like auth tokens or accountIds - you can usually just edit those out manually and replace with say `XXX`

## 'API Data failed to parse' diagnostics

If you have an error starting with:

`API Data failed to parse.`

* Please install googleAuthR >v0.7.9000:

```r
remotes::install_github("MarkEdmondson1234/googleAuthR")
```

* Restart the R session and run your bugged code.
* It should write a file called `gar_parse_error.rds` to your working directory.
* Run the below to output some diagnostic information and paste the response here.

```r
googleAuthR::gar_debug_parsing("gar_parse_error.rds")
```

## Session Info

Please run `sessionInfo()` so we can check what versions of packages you have installed
