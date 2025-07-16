#'
#' Retrieve original articles of DAFNEE journals in 2023
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

options(openalexR.mailto = 'nicolas.casjaus@fondationbiodiversite.fr')

## Import DAFNEE journals ----

journals <- gsheet::gsheet2tbl(
  url = paste0(
    "https://docs.google.com/spreadsheets/d/",
    "1lsk3JrJsv4WJbI0Btf5NXFH61R6YYAS2LMezrMxVshc/edit?",
    "gid=2099703920#gid=2099703920"
  )
) |>
  as.data.frame()

journals <- journals[, c("oa_source_id", "oa_source_name", "finalvote")]


## Remove DAFNEE journals absent from OA ----

journals <- journals[journals$"finalvote" != "excluded", ]
journals <- journals[!is.na(journals$"oa_source_id"), ]


## Get original articles metadata ----

for (i in 1:nrow(journals)) {
  cat("Retrieving original papers for journal", i, "on", nrow(journals), "\r")

  articles <- oa_get_original_papers(
    journal_id = journals[i, "oa_source_id"],
    year = 2023
  )

  journal_id <- gsub("https://openalex.org/", "", journals[i, "oa_source_id"])

  qs::qsave(
    x = articles,
    file = here::here("outputs", "original_papers", paste0(journal_id, ".qs"))
  )
}
