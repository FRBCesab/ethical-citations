#'
#' Compute NP and FP citations ratios per journal and per publisher type
#'

## Import Dafnee info ----

dafnee <- read.csv(
  file = here::here("data", "derived-data", "final_list_of_journals.csv")
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

pos <- which(
  citations$"business_model" %in% c("University Press", "Non-profit", "NP")
)

if (length(pos) > 0) {
  citations[pos, "publisher_type"] <- "NP"
}

pos <- which(citations$"business_model" %in% c("For-profit", "FP"))

if (length(pos) > 0) {
  citations[pos, "publisher_type"] <- "FP"
}


## Remove original papers w/ no citations (found in Dafnee) ----

citations <- citations[citations$"n_refs" > citations$"na_dafnee", ]


## Remove original papers w/ less than 5 citations (found in Dafnee) ----

citations <- citations[(citations$"n_refs" - citations$"na_dafnee_na") > 5, ]

citations <- as.data.frame(citations)


## Compute NP ratios for NP Journals ----

get_ratios <- function(data, business_model, is_academic) {
  data <- data[
    which(
      data$"publisher_type" == business_model & data$"is_dafnee" == is_academic
    ),
  ]

  total <- data$fp_nonacademic +
    data$fp_academic +
    data$np_academic +
    data$np_nonacademic

  data$fp_nonacademic <- data$fp_nonacademic / total
  data$fp_academic <- data$fp_academic / total
  data$np_nonacademic <- data$np_nonacademic / total
  data$np_academic <- data$np_academic / total

  fp_nonacademic <- tapply(
    data$"fp_nonacademic",
    INDEX = data$"oa_source_id",
    mean
  )

  fp_academic <- tapply(
    data$"fp_academic",
    INDEX = data$"oa_source_id",
    mean
  )

  np_nonacademic <- tapply(
    data$"np_nonacademic",
    INDEX = data$"oa_source_id",
    mean
  )

  np_academic <- tapply(
    data$"np_academic",
    INDEX = data$"oa_source_id",
    mean
  )

  data <- data.frame(
    "oa_source_id" = names(fp_nonacademic),
    business_model = business_model,
    is_academic = is_academic,
    fp_nonacademic,
    fp_academic,
    np_nonacademic,
    np_academic
  )

  rownames(data) <- NULL

  data
}

res1 <- get_ratios(citations, business_model = "NP", is_academic = TRUE)
res2 <- get_ratios(citations, business_model = "NP", is_academic = FALSE)
res3 <- get_ratios(citations, business_model = "FP", is_academic = TRUE)
res4 <- get_ratios(citations, business_model = "FP", is_academic = FALSE)


ratio_per_journals <- rbind(res1, res2, res3, res4)

write.csv(
  x = ratio_per_journals,
  file = here::here("outputs", "mean_ratio_per_journal.csv"),
  row.names = FALSE
)


ratios <- data.frame(
  rbind(
    apply(res1[, -c(1:3)], 2, mean),
    apply(res2[, -c(1:3)], 2, mean),
    apply(res3[, -c(1:3)], 2, mean),
    apply(res4[, -c(1:3)], 2, mean)
  )
)

ratios$"business_model" <- c("NP", "NP", "FP", "FP")
ratios$"is_academic" <- c(TRUE, FALSE, TRUE, FALSE)
ratios$"n_journals" <- c(nrow(res1), nrow(res2), nrow(res3), nrow(res4))


write.csv(
  x = ratios,
  file = here::here("outputs", "mean_ratio_per_publisher_type.csv"),
  row.names = FALSE
)


# ratio_per_journals <- rbind(
#   ratio_per_journals,
#   data.frame(
#     "publisher_type" = "Non-profit",
#     "oa_source_id" = names(np_np_ratio),
#     "np_mean" = round(np_np_ratio, 2),
#     "fp_mean" = 1 - round(np_np_ratio, 2)
#   )
# )

# ratios[1, "np_mean"] <- round(mean(np_np_ratio), 2)
# ratios[1, "np_sd"] <- round(sd(np_np_ratio), 2)

# ## Compute FP ratios for NP Journals ----

# fp_ratio <- np_journals$"fp" / (np_journals$"np" + np_journals$"fp")

# np_fp_ratio <- data.frame("oa_source_id" = np_journals$"oa_source_id", fp_ratio)
# np_fp_ratio <- tapply(
#   np_fp_ratio$"fp_ratio",
#   INDEX = np_fp_ratio$"oa_source_id",
#   mean
# )

# ratios[1, "fp_mean"] <- round(mean(np_fp_ratio), 2)
# ratios[1, "fp_sd"] <- round(sd(np_fp_ratio), 2)

# ## Compute FP ratios for FP Journals----

# fp_journals <- citations[citations$"publisher_type" == "For-profit", ]

# fp_ratio <- fp_journals$"fp" / (fp_journals$"np" + fp_journals$"fp")

# fp_fp_ratio <- data.frame("oa_source_id" = fp_journals$"oa_source_id", fp_ratio)
# fp_fp_ratio <- tapply(
#   fp_fp_ratio$"fp_ratio",
#   INDEX = fp_fp_ratio$"oa_source_id",
#   mean
# )

# ratio_per_journals <- rbind(
#   ratio_per_journals,
#   data.frame(
#     "publisher_type" = "For-profit",
#     "oa_source_id" = names(fp_fp_ratio),
#     "np_mean" = 1 - round(fp_fp_ratio, 2),
#     "fp_mean" = round(fp_fp_ratio, 2)
#   )
# )

# ratios[2, "fp_mean"] <- round(mean(fp_fp_ratio), 2)
# ratios[2, "fp_sd"] <- round(sd(fp_fp_ratio), 2)

# ## Compute NP ratios for FP Journals----

# np_ratio <- fp_journals$"np" / (fp_journals$"np" + fp_journals$"fp")

# fp_np_ratio <- data.frame("oa_source_id" = fp_journals$"oa_source_id", np_ratio)
# fp_np_ratio <- tapply(
#   fp_np_ratio$"np_ratio",
#   INDEX = fp_np_ratio$"oa_source_id",
#   mean
# )

# ratios[2, "np_mean"] <- round(mean(fp_np_ratio), 2)
# ratios[2, "np_sd"] <- round(sd(fp_np_ratio), 2)

# rownames(ratio_per_journals) <- NULL

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
