FROM rocker/shiny

COPY app.R /srv/shiny-server/app.R
COPY app/ /srv/shiny-server/app/

COPY shiny-customised.config /etc/shiny-server/shiny-server.conf

EXPOSE 8080

USER shiny

# avoid s6 initialization
# see https://github.com/rocker-org/shiny/issues/79
CMD ["/usr/bin/shiny-server"]