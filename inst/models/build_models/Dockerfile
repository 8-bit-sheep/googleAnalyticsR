FROM gcr.io/gcer-public/googleanalyticsr

RUN install2.r --error \ 
    -r 'http://cran.rstudio.com' \
    xts dygraphs CausalImpact shiny plotly \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
    