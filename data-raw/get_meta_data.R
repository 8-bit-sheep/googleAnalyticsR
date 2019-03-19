## script to get meta data, rerun each release
library(googleAnalyticsR)
#ga_auth()
meta <- ga_meta()
devtools::use_data(meta, overwrite = TRUE)
devtools::use_data_raw()
