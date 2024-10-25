#' ethical-citations: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Nicolas Casajus \email{rdev.nc@gmail.com}
#' 
#' @date 2024/07/09



## Install Dependencies (listed in DESCRIPTION) ----

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

pkgload::load_all(here::here())


## Global Variables ----

options(openalexR.mailto = "rdev.nc@gmail.com")


## Run Project ----

# List all R scripts in a sequential order and using the following form:
source(here::here("analyses", "1-get_journal_info_in_oa.R"))
source(here::here("analyses", "2-get_original_papers_in_oa.R"))
source(here::here("analyses", "3-get_cited_references_in_oa.R"))
