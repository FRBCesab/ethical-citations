#'
#' Retrieve DAFNEE journals metadata in OpenAlex database
#'

## Read DAFNEE database ----

dafnee_journals <- read.csv(here::here(
  "data",
  "raw-data",
  "DAFNEE-20241022.csv"
))


## Clean column names -----

dafnee_journals <- janitor::clean_names(dafnee_journals)


## Select journals based on fields ----

fields <- c("ecology", "evolution/systematics", "general", "organisms")

dafnee_journals <- dafnee_journals[dafnee_journals$"field" %in% fields, ]


## Check for duplicates in journal names ----

any(duplicated(dafnee_journals$"journal"))


## Clean some journal names ----

dafnee_journals$"journal_clean" <- dafnee_journals$"journal"

dafnee_journals$"journal_clean" <-
  gsub(
    "Proceedings of the National Academy of Sciences of the USA",
    "Proceedings of the National Academy of Sciences",
    dafnee_journals$"journal_clean"
  )


## Retrieve OA journal metadata (including ISSN) ----

oa_data <- data.frame()

for (i in 1:nrow(dafnee_journals)) {
  cat("  OA - Retrieving Journal Info -", i, "on", nrow(dafnee_journals), "\r")

  oa_data <- rbind(
    oa_data,
    oa_get_journal_info(dafnee_journals[i, "journal_clean"])
  )
}


## Append results ----

dafnee_journals <- merge(
  x = dafnee_journals,
  y = oa_data,
  by.x = "journal_clean",
  by.y = "journal",
  all = TRUE
)


## Percentage of journals absent from OA ----

round(
  100 * sum(is.na(dafnee_journals$"oa_works_count")) / nrow(dafnee_journals),
  0
)


## Export data ----

write.csv(
  dafnee_journals,
  file = here::here("data", "derived-data", "DAFNEE_db_with_issn.csv"),
  row.names = FALSE
)

## Important! Some journal names must be fixed manually!
