# ---start header_boilerplate.R
library(shiny)
library(googleAuthR)
library(googleAnalyticsR)
{{{ model_libraries }}}

gar_set_client(web_json = "{{ web_json }}",
               scopes = "{{ scopes }}",
               activate = "web")
options(googleAuthR.redirect = "{{ deployed_url }}")

# loads pre-existing models
{{{ model_load }}}
# ---end header_boilerplate.R

