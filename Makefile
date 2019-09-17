# R parameters
RCMD=Rscript -e

all: test release
test:
  $(RCMD) rhub::check_for_cran()
	$(RCMD) devtools::test()
	$(RCMD) covr::codecov(line_exclusions = list("R/gadget.R","R/shiny-modules.R","R/shiny-modules-segments.R","R/options.R"))

release: