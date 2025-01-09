#' Retrieve original articles of DAFNEE Journals in 2023
#' 
#' This script exports a 'qs' object per journal in outputs/original_papers/.
#' Exported 'data.frame' contain one row per original article and the 
#' following columns:
#' - `oa_source_id`: the OpenAlex identifier of the journal
#' - `oa_work_id`: the OpenAlex identifier of the original article
#' - `oa_work_doi`: the DOI of the original article
#' - `oa_work_year`: the publication year of the original article
#' - `oa_referenced_work_id`: the OpenAlex identifier of the cited works 
#'   (collapsed in one string using ' | ' as separator)


## Import DAFNEE journals ----

journals <- read.csv(here::here("data", "derived-data", 
                                "DAFNEE_db_with_issn.csv"))


## Remove DAFNEE journals absent from OA ----

journals <- journals[!is.na(journals$"oa_source_id"), ]


## Get original articles metadata ----

for (i in 1:nrow(journals)) {
  
  cat("Retrieving original papers for journal", i, "on", nrow(journals), "\r")
  
  articles <- oa_get_original_papers(journal_id = journals[i, "oa_source_id"], 
                                     year       = 2023)
  
  journal_id <- gsub("https://openalex.org/", "", journals[i, "oa_source_id"])
  
  qs::qsave(x    = articles,
            file = here::here("outputs", "original_papers", 
                              paste0(journal_id, ".qs")))
}
