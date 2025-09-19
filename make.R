#' ethical-citations: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#'
#' @date 2025/06/06

## Install dependencies (listed in DESCRIPTION) ----

remotes::install_deps(upgrade = "never")


## Load project addins (R functions and packages) ----

pkgload::load_all()


## Global variables ----

options(openalexR.mailto = "rdev.nc@gmail.com")


## Run project ----

source(here::here("analyses", "1-get_original_papers_in_oa.R"))
source(here::here("analyses", "2-get_cited_references_in_oa.R"))
source(here::here("analyses", "3-aggregate_cited_references.R"))
source(here::here("analyses", "4-compute_mean_ratios.R"))
source(here::here("analyses", "5-violin_plots.R"))
source(here::here("analyses", "7-stats.R")) ###
