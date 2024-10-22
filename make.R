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

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

options(openalexR.mailto = "nicolas.casajus@fondationbiodiversite.fr")


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))
