FROM gcr.io/gcer-public/googleanalyticsr

RUN ulimit -s 16384 \
    && R -e 'install.packages("Boom");'

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    xts dygraphs \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    CausalImpact \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    shiny plotly \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds