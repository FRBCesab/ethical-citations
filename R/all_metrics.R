#' Number of selected journals (without any filter)
#'
#' @noRd

number_of_original_journals <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  nrow(journals)
}


#'
#' @noRd

number_of_original_journals_per_publisher_type <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  metrics <- tapply(
    journals$"oa_source_name",
    list(journals$"business_model", journals$"is_dafnee"),
    length,
    default = 0
  )

  non_profit_journals <- sum(metrics[2:3, ])
  for_profit_acad_journals <- metrics[1, 2]
  for_profit_nonacad_journals <- metrics[1, 1]

  list(
    "non_profit_journals" = non_profit_journals,
    "for_profit_acad_journals" = for_profit_acad_journals,
    "for_profit_nonacad_journals" = for_profit_nonacad_journals
  )
}


#'
#' @noRd

number_of_original_articles <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

  length(unique(citations$"oa_work_id"))
}


#'
#' @noRd

number_of_cited_references <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

  sum(citations$"n_refs")
}


#'
#' @noRd

number_of_filtered_journals <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ]

  length(unique(citations$"oa_source_id"))
}


#'
#' @noRd

number_of_filtered_journals_per_publisher_type <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ]

  citations <- citations[!duplicated(citations$"oa_source_id"), ]

  metrics <- tapply(
    citations$"oa_source_id",
    list(citations$"publisher_type", citations$"is_dafnee"),
    length,
    default = 0
  )

  non_profit_journals <- sum(metrics[2, ])
  for_profit_acad_journals <- metrics[1, 2]
  for_profit_nonacad_journals <- metrics[1, 1]

  list(
    "non_profit_journals" = non_profit_journals,
    "for_profit_acad_journals" = for_profit_acad_journals,
    "for_profit_nonacad_journals" = for_profit_nonacad_journals
  )
}


#'
#' @noRd

number_of_filtered_original_articles <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ]

  length(unique(citations$"oa_work_id"))
}


#'
#' @noRd

number_of_filtered_cited_references <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ]

  sum(citations$"n_refs" - citations$"na_dafnee")
}


#'
#' @noRd

mean_number_cited_references_per_article <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ]

  paste0(
    round(mean(citations$"n_refs" - citations$"na_dafnee"), 2),
    " (+/- ",
    round(sd(citations$"n_refs" - citations$"na_dafnee"), 2),
    " SD)"
  )
}

#'
#' @noRd

number_original_dafnee_journals <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  table(journals$"is_dafnee")[["TRUE"]]
}

#'
#' @noRd

number_original_additional_journals <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  table(journals$"is_dafnee")[["FALSE"]]
}


#'
#' @noRd

median_number_of_original_articles_per_journal <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

  tapply(
    citations$"oa_work_id",
    citations$"oa_source_id",
    length,
    default = 0
  ) |>
    median()
}


#'
#' @noRd

mean_number_of_original_articles_per_journal <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

  metrics <- tapply(
    citations$"oa_work_id",
    citations$"oa_source_id",
    length,
    default = 0
  )

  paste0(
    round(mean(metrics), 2),
    " (+/- ",
    round(sd(metrics), 2),
    " SD)"
  )
}


#'
#' @noRd

median_number_of_cited_references_per_articles <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

  median(citations$"n_refs")
}


#'
#' @noRd

mean_number_of_cited_references_per_articles <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

  paste0(
    round(mean(citations$"n_refs"), 2),
    " (+/- ",
    round(sd(citations$"n_refs"), 2),
    " SD)"
  )
}


#'
#' @noRd

number_of_articles_w_refs_in_dafnee_db <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  length(unique(citations$"oa_work_id"))
}


#'
#' @noRd

number_of_cited_references_in_dafnee_db <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  sum(citations$"n_refs" - citations$"na_dafnee")
}


#'
#' @noRd

number_of_filtered_articles_per_publisher_type <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ]

  metrics <- tapply(
    citations$"oa_work_id",
    list(citations$"publisher_type", citations$"is_dafnee"),
    length,
    default = 0
  )

  non_profit_articles <- sum(metrics[2, ])
  for_profit_acad_articles <- metrics[1, 2]
  for_profit_nonacad_articles <- metrics[1, 1]
  total_articles <- sum(metrics)

  list(
    "non_profit_articles" = non_profit_articles,
    "for_profit_acad_articles" = for_profit_acad_articles,
    "for_profit_nonacad_articles" = for_profit_nonacad_articles,
    "total_articles" = total_articles
  )
}


#'
#' @noRd

number_of_filtered_references_per_publisher_type <- function() {
  journals <- read.csv(
    file = here::here("data", "raw-data", "list_of_selected_journals.csv")
  )

  journals <- journals[!duplicated(journals$"oa_source_name"), ]

  ## Import aggregated references ----

  citations <- readRDS(
    file = here::here(
      "outputs",
      "number_of_cited_references_per_paper.rds"
    )
  )

  citations <- do.call(rbind, citations)

  ## Add Dafnee journal information ----

  citations <- merge(citations, journals, by = "oa_source_id", all = FALSE)

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

  citations <- citations[(citations$"n_refs" - citations$"na_dafnee") > 5, ]

  metrics <- tapply(
    (citations$"n_refs" - citations$"na_dafnee"),
    list(citations$"publisher_type", citations$"is_dafnee"),
    sum,
    default = 0
  )

  non_profit_refs <- sum(metrics[2, ])
  for_profit_acad_refs <- metrics[1, 2]
  for_profit_nonacad_refs <- metrics[1, 1]
  total_refs <- sum(metrics)

  list(
    "non_profit_refs" = non_profit_refs,
    "for_profit_acad_refs" = for_profit_acad_refs,
    "for_profit_nonacad_refs" = for_profit_nonacad_refs,
    "total_refs" = total_refs
  )
}
