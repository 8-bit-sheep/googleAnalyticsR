library(knitr)
# run this locally to creatae Rmd output with local API auth

knitr::knit("vignettes/management.Rmd.raw", "vignettes/management.Rmd")
knitr::knit("vignettes/models.Rmd.raw", "vignettes/models.Rmd")
