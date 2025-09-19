#'
#' Compute NP and FP citations ratios per journal and per publisher type
#'

## Import Dafnee info ----

dafnee <- read.csv(
  file = here::here("data", "raw-data", "list_of_selected_journals.csv")
)

dafnee <- dafnee[!duplicated(dafnee$"oa_source_name"), ]


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

citations <- citations[citations$"n_refs" > citations$"na_dafnee", ] #losing: 19470 refs


## Remove original papers w/ less than 5 citations (found in Dafnee) ----

citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ] #losing: 40326 refs

citations <- as.data.frame(citations)


citations$"np" <- citations$"np_academic" + citations$"np_nonacademic"

pos <- which(citations$publisher_type == "NP")

if (length(pos) > 0) {
  citations[pos, "is_dafnee"] <- TRUE
}


## Compute NP ratios for NP Journals ----

get_ratios <- function(data, business_model, is_academic) {
  data <- data[
    which(
      data$"publisher_type" == business_model & data$"is_dafnee" == is_academic
    ),
  ]

  total <- data$fp_nonacademic + data$fp_academic + data$np

  data$fp_nonacademic <- data$fp_nonacademic / total
  data$fp_academic <- data$fp_academic / total
  data$np <- data$np / total

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

  np <- tapply(
    data$"np",
    INDEX = data$"oa_source_id",
    mean
  )

  data <- data.frame(
    "oa_source_id" = names(fp_nonacademic),
    business_model = business_model,
    is_academic = is_academic,
    fp_nonacademic,
    fp_academic,
    np
  )

  rownames(data) <- NULL

  data
}

res1 <- get_ratios(citations, business_model = "NP", is_academic = TRUE)
res2 <- get_ratios(citations, business_model = "FP", is_academic = TRUE)
res3 <- get_ratios(citations, business_model = "FP", is_academic = FALSE)


ratio_per_journals <- rbind(res1, res2, res3)

write.csv(
  x = ratio_per_journals,
  file = here::here("outputs", "mean_ratio_per_journal.csv"),
  row.names = FALSE
)


ratios <- data.frame(
  rbind(
    apply(res1[, -c(1:3)], 2, mean),
    apply(res2[, -c(1:3)], 2, mean),
    apply(res3[, -c(1:3)], 2, mean)
  )
)

ratios$"business_model" <- c("NP", "FP", "FP")
ratios$"is_academic" <- c(TRUE, TRUE, FALSE)
ratios$"n_journals" <- c(nrow(res1), nrow(res2), nrow(res3))
ratios$var <- 'mean ratio'

SDs <- data.frame(
  rbind(
    apply(res1[, -c(1:3)], 2, sd),
    apply(res2[, -c(1:3)], 2, sd),
    apply(res3[, -c(1:3)], 2, sd)
  )
)

SDs$"business_model" <- c("NP", "FP", "FP")
SDs$"is_academic" <- c(TRUE, TRUE, FALSE)
SDs$"n_journals" <- c(nrow(res1), nrow(res2), nrow(res3))
SDs$var <- 'sd'

all <- rbind(ratios, SDs)

write.csv(
  x = ratios,
  file = here::here("outputs", "mean_ratio_per_publisher_type.csv"),
  row.names = FALSE
)
