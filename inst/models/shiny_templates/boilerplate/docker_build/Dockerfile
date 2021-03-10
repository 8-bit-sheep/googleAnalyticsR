# from rocker/shiny with googleAnalyticsR installed & configued for Cloud Run
FROM rocker/shiny:4.0.4

# install R package dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev \
    ## clean up
    && apt-get clean \ 
    && rm -rf /var/lib/apt/lists/ \ 
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
## Install packages from CRAN
RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    remotes curl httr gargle googleAuthR cli \
    ## install Github packages
    && installGithub.r MarkEdmondson1234/googleAuthR \
    ## clean up
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    
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

CMD ["/usr/bin/shiny-server"]