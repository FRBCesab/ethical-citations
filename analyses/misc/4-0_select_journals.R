dafnee_journals <- read.csv(here::here(
  "data",
  "raw-data",
  "DAFNEE-20250701.csv"
))

fields <- c(
  "applied_ecology",
  "general",
  "basic_ecology",
  "evolution"
)

dafnee_journals <- dafnee_journals[dafnee_journals$"Field" %in% fields, ]


oa_journals <- read.csv(here::here(
  "data",
  "derived-data",
  "DAFNEE_db_with_issn.csv"
))


dafnee_journals <- oa_journals[
  which(oa_journals$"journal" %in% dafnee_journals$"Journal"),
]


dafnee_journals <- dafnee_journals[, c(
  "oa_source_id",
  "oa_source_name",
  "publisher_type"
)]

colnames(dafnee_journals)[3] <- "business_model"

dafnee_journals$"is_dafnee" <- TRUE

nondafnee_journals <- gsheet::gsheet2tbl(
  url = paste0(
    "https://docs.google.com/spreadsheets/d/",
    "1lsk3JrJsv4WJbI0Btf5NXFH61R6YYAS2LMezrMxVshc/edit?",
    "gid=2099703920#gid=2099703920"
  )
) |>
  as.data.frame()

nondafnee_journals <- nondafnee_journals[
  nondafnee_journals$"final_final" == "included",
]

nondafnee_journals <- nondafnee_journals[, c(
  "oa_source_id",
  "oa_source_name",
  "final_business"
)]

colnames(nondafnee_journals)[3] <- "business_model"

nondafnee_journals$"is_dafnee" <- FALSE

final_list_of_journals <- rbind(dafnee_journals, nondafnee_journals)

final_list_of_journals <- final_list_of_journals[
  !is.na(final_list_of_journals$oa_source_id),
]

write.csv(
  final_list_of_journals,
  here::here("data", "derived-data", "final_list_of_journals.csv"),
  row.names = FALSE
)


## Retrieve original papers for new journals ----

articles <- oa_get_original_papers(
  journal_id = final_list_of_journals[1, "oa_source_id"],
  year = 2023
)

journal_id <- gsub(
  "https://openalex.org/",
  "",
  final_list_of_journals[1, "oa_source_id"]
)

qs::qsave(
  x = articles,
  file = here::here("outputs", "original_papers", paste0(journal_id, ".qs"))
)


## Retrieve cited references for new journals ----

file_name <- here::here("outputs", "original_papers", paste0(journal_id, ".qs"))

original_articles <- qs::qread(file_name)

if (nrow(original_articles) > 0) {
  original_articles <- str_to_list(
    data = original_articles,
    column = "oa_referenced_work_id"
  )

  referenced_works <- oa_get_work_metadata(
    work_id = original_articles$"oa_referenced_work_id",
    mc_cores = 20
  )

  referenced_works <- merge(
    original_articles,
    referenced_works,
    by = "oa_referenced_work_id",
    all = TRUE
  )

  referenced_works <- referenced_works[, c(2:5, 1, 6)]

  journal_id <- gsub(
    "https://openalex.org/",
    "",
    referenced_works[1, "oa_source_id"]
  )

  qs::qsave(
    x = referenced_works,
    file = here::here(
      "outputs",
      "cited_references",
      paste0(journal_id, ".qs")
    )
  )
}
