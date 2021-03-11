library(googleCloudRunner)

repo <- cr_buildtrigger_repo("MarkEdmondson1234/googleAnalyticsR", 
                             branch = "master")

cr_deploy_docker_trigger(
  repo,
  image = "googleanalyticsr-shiny-cloudrun",
  location = "inst/models/shiny_templates/boilerplate/docker_build/",
  project = "gcer-public"
)