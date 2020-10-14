FROM gcr.io/gcer-public/googleauthr:master

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    googleAnalyticsR \
    ## install Github packages
    && installGithub.r MarkEdmondson1234/googleAnalyticsR \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds