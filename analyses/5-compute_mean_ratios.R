#'
#' Compute NP and FP citations ratios per journal and per publisher type
#'

## Import Dafnee info ----

dafnee <- read.csv(
  file = here::here(
    "data",
    "derived-data",
    "DAFNEE_db_with_issn.csv"
  )
)


## Import aggregated references ----

citations <- readRDS(
  file = here::here(
    "outputs",
    "number_of_cited_references_per_paper.rds"
  )
)

citations <- do.call(rbind, citations)


## Add Dafnee journal information ----

citations <- merge(citations, dafnee, by = "oa_source_id", all = FALSE)

pos <- which(citations$"publisher_type" == "University Press")
if (length(pos) > 0) {
  citations[pos, "publisher_type"] <- "Non-profit"
}


## Remove original papers w/ no citations (found in Dafnee) ----

citations <- citations[citations$"n_refs" > citations$"na_dafnee", ]


## Remove original papers w/ less than 5 citations (found in Dafnee) ----

citations <- citations[(citations$"fp" + citations$"np") > 5, ]

citations <- as.data.frame(citations)

n_journals <- length(unique(citations$"oa_source_id"))

ratio_per_journals <- data.frame()

ratios <- data.frame(
  "publisher_type" = c("Non-Profit", "For-Profit"),
  "np_mean" = integer(2),
  "np_sd" = integer(2),
  "fp_mean" = integer(2),
  "fp_sd" = integer(2)
)


## Compute NP ratios for NP Journals ----

np_journals <- citations[citations$"publisher_type" == "Non-profit", ]

np_ratio <- np_journals$"np" / (np_journals$"np" + np_journals$"fp")

np_np_ratio <- data.frame("oa_source_id" = np_journals$"oa_source_id", np_ratio)
np_np_ratio <- tapply(
  np_np_ratio$"np_ratio",
  INDEX = np_np_ratio$"oa_source_id",
  mean
)

ratio_per_journals <- rbind(
  ratio_per_journals,
  data.frame(
    "publisher_type" = "Non-profit",
    "oa_source_id" = names(np_np_ratio),
    "np_mean" = round(np_np_ratio, 2),
    "fp_mean" = 1 - round(np_np_ratio, 2)
  )
)

ratios[1, "np_mean"] <- round(mean(np_np_ratio), 2)
ratios[1, "np_sd"] <- round(sd(np_np_ratio), 2)


## Compute FP ratios for NP Journals ----

fp_ratio <- np_journals$"fp" / (np_journals$"np" + np_journals$"fp")

np_fp_ratio <- data.frame("oa_source_id" = np_journals$"oa_source_id", fp_ratio)
np_fp_ratio <- tapply(
  np_fp_ratio$"fp_ratio",
  INDEX = np_fp_ratio$"oa_source_id",
  mean
)

ratios[1, "fp_mean"] <- round(mean(np_fp_ratio), 2)
ratios[1, "fp_sd"] <- round(sd(np_fp_ratio), 2)


## Compute FP ratios for FP Journals----

fp_journals <- citations[citations$"publisher_type" == "For-profit", ]

fp_ratio <- fp_journals$"fp" / (fp_journals$"np" + fp_journals$"fp")

fp_fp_ratio <- data.frame("oa_source_id" = fp_journals$"oa_source_id", fp_ratio)
fp_fp_ratio <- tapply(
  fp_fp_ratio$"fp_ratio",
  INDEX = fp_fp_ratio$"oa_source_id",
  mean
)

ratio_per_journals <- rbind(
  ratio_per_journals,
  data.frame(
    "publisher_type" = "For-profit",
    "oa_source_id" = names(fp_fp_ratio),
    "np_mean" = 1 - round(fp_fp_ratio, 2),
    "fp_mean" = round(fp_fp_ratio, 2)
  )
)

ratios[2, "fp_mean"] <- round(mean(fp_fp_ratio), 2)
ratios[2, "fp_sd"] <- round(sd(fp_fp_ratio), 2)


## Compute NP ratios for FP Journals----

np_ratio <- fp_journals$"np" / (fp_journals$"np" + fp_journals$"fp")

fp_np_ratio <- data.frame("oa_source_id" = fp_journals$"oa_source_id", np_ratio)
fp_np_ratio <- tapply(
  fp_np_ratio$"np_ratio",
  INDEX = fp_np_ratio$"oa_source_id",
  mean
)

ratios[2, "np_mean"] <- round(mean(fp_np_ratio), 2)
ratios[2, "np_sd"] <- round(sd(fp_np_ratio), 2)

rownames(ratio_per_journals) <- NULL


## Export results ----

write.csv(
  x = ratio_per_journals,
  file = here::here("outputs", "mean_ratio_per_journal.csv"),
  row.names = FALSE
)


write.csv(
  x = ratios,
  file = here::here("outputs", "mean_ratio_per_publisher_type.csv"),
  row.names = FALSE
)
