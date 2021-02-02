FROM gcr.io/gcer-public/googleanalyticsr

RUN ulimit -s 16384 \
    && R -e 'install.packages("Boom");'

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    xts dygraphs

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    CausalImpact

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    shiny plotly

RUN installGithub.r MarkEdmondson1234/googleAnalyticsR \
    && R -e 'sessionInfo()' \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds