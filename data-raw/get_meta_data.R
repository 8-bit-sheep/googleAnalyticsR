## script to get meta data, rerun each release
library(googleAnalyticsR)
#ga_auth()
meta <- ga_meta()
usethis::use_data(meta, overwrite = TRUE)

#ga4 meta
meta4 <- ga_meta("data")
usethis::use_data(meta4, overwrite = TRUE)
