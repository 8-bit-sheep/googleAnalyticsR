library(googleCloudRunner)

# setup test client id and client secret
#googleAnalyticsR::ga_auth_setup()

auth_steps <- c(cr_buildstep_secret("googleanalyticsr-tests", 
                                    "/workspace/auth.json"),
                cr_buildstep_secret("mark-edmondson-gde-clientid", 
                                    "/workspace/client.json")
                )
auth_env <- c("GA_AUTH_FILE=/workspace/auth.json",
              "GAR_CLIENT_JSON=/workspace/client.json")

cr_deploy_packagetests(
  steps = auth_steps,
  cloudbuild_file = "cloud_build/cloudbuild-tests.yml",
  timeout = 2400,
  env = c("NOT_CRAN=true",auth_env),
  build_image = "gcr.io/mark-edmondson-gde/googleanalyticsr-tests:master"
)

cr_deploy_pkgdown(
  steps = auth_steps,
  secret = "github-ssh",
  github_repo = "MarkEdmondson1234/googleAnalyticsR",
  cloudbuild_file = "cloud_build/cloudbuild-pkgdown.yml",
  build_image = "gcr.io/mark-edmondson-gde/googleanalyticsr-tests:master",
  env = auth_env
)
