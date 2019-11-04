## script to get meta data, rerun each release
library(googleAnalyticsR)
#ga_auth()
meta <- ga_meta()
usethis::use_data(meta, overwrite = TRUE)
