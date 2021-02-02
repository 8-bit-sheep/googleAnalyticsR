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
  env = c("NOT_CRAN=true",auth_env)
)

cr_deploy_pkgdown(
  steps = auth_steps,
  secret = "github-ssh",
  github_repo = "MarkEdmondson1234/googleAnalyticsR",
  cloudbuild_file = "cloud_build/cloudbuild-pkgdown.yml",
  env = auth_env
)

# deploy Docker to github packages

# run build of models
bs <- c(
  cr_buildstep_docker(
    "ga-model-examples",
    dir = "inst/models/build_models/",
    kaniko_cache = TRUE
  ),
  cr_buildstep_gitsetup("github-ssh"),
  cr_buildstep_git(
    git_args = c("clone",
                 "git@github.com:MarkEdmondson1234/googleAnalyticsR",
                 "payload")),
  cr_buildstep_r(
    "inst/models/build_models/ga_model_makes.R",
    name = "gcr.io/mark-edmondson-gde/ga-model-examples:$BUILD_ID",
    dir = "payload"),
  cr_buildstep_git(git_args = c("add","--all"), 
                   dir = "payload"),
  cr_buildstep_git(
    git_args = c("commit", "-a","-m","build_models from ${COMMIT_SHA}"),
    dir = "payload"),
  cr_buildstep_git(git_args = c("push"),
                   dir = "payload")
)

by <- cr_build_yaml(bs, timeout = 2400)
## if building Boom/CausalImpact you will need a bigger machine
#build <- cr_build_make(by, options = list(machineType = "E2_HIGHCPU_8"))
## but this one is cheaper
build <- cr_build_make(by)

cr_build_write(build, "cloud_build/build_models.yml")

cr_buildtrigger(
  "cloud_build/build_models.yml", 
  name = "ga-model-example-builds",
  trigger = cr_buildtrigger_repo("MarkEdmondson1234/googleAnalyticsR"))
