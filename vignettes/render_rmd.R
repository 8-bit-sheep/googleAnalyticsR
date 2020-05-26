library(knitr)
library(webshot)
webshot::install_phantomjs()
# run this locally to create Rmd output with local API auth
wd <- getwd()
setwd("vignettes")
knitr::knit("management.Rmd.raw", "management.Rmd")
knitr::knit("models.Rmd.raw", "models.Rmd")
setwd(wd)