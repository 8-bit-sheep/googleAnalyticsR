library(googleCloudRunner)

# setup test client id and client secret
#googleAnalyticsR::ga_auth_setup()

cr_deploy_packagetests(
  steps = cr_buildstep_secret("googleanalyticsr-tests", "/workspace/auth.json"),
  cloudbuild_file = "cloud_build/cloudbuild-tests.yml",
  timeout = 2400,
  env = c("NOT_CRAN=true","GA_AUTH_FILE=/workspace/auth.json")
)

cr_deploy_pkgdown(
  steps = cr_buildstep_secret("googleanalyticsr-tests", "/workspace/auth.json"),
  secret = "github-ssh",
  github_repo = "MarkEdmondson1234/googleAnalyticsR",
  cloudbuild_file = "cloud_build/cloudbuild-pkgdown.yml",
  env = "GA_AUTH_FILE=/workspace/auth.json"
)
