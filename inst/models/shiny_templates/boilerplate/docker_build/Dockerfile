FROM rocker/shiny

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    googleAnalyticsR \
    ## install Github packages
    && installGithub.r MarkEdmondson1234/googleAnalyticsR \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

COPY shiny-customised.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]