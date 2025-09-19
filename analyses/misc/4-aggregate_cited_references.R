#'
#' Compute number of citations per article
#'
#' In particular:
#' - n_refs: total number of citations indexed in OA database
#' - np: number of citations of non-profit journals
#' - fp: number of citations of for-profit journals
#' - na_dafnee: number of citations of journals missing in Dafnee database

## Import Dafnee info ----

dafnee <- read.csv(
  file = here::here("data", "derived-data", "final_list_of_journals.csv")
)


## Clean Dafnee info ----

# dafnee <- dafnee[, c(
#   "oa_source_id",
#   "oa_source_name",
#   "journal",
#   "publisher_type",
#   "business_model",
#   "institution_type"
# )]

dafnee <- dafnee[!is.na(dafnee$"oa_source_id"), ]

dafnee <- dafnee[!duplicated(dafnee$oa_source_name), ]

pos <- which(dafnee$"business_model" == "University Press")
if (length(pos) > 0) {
  dafnee[pos, "business_model"] <- "Non-profit"
}

dafnee <- dafnee |>
  dplyr::mutate(
    publisher_type = dplyr::case_when(
      business_model == 'For-profit' ~ 'FP',
      business_model == 'Non-profit' ~ 'NP',
      business_model == 'FP' ~ 'FP',
      business_model == 'NP' ~ 'NP'
    )
  )

dafnee <- dafnee |>
  dplyr::mutate(
    is_dafnee = dplyr::case_when(
      is_dafnee == TRUE ~ 'academic',
      is_dafnee == FALSE ~ 'nonacademic'
    )
  )


## List cited reference files ----

list_refsfiles <- list.files(
  path = here::here("outputs", "cited_references"),
  full.names = TRUE
)


oa_source_id <- paste0(
  gsub("https://openalex.org/", "", dafnee$oa_source_id),
  ".qs"
)

list_refsfiles <- list_refsfiles[which(
  basename(list_refsfiles) %in% oa_source_id
)]


## Compute number of citations per article ----

res_list <- list()

for (i in 1:length(list_refsfiles)) {
  df <- qs::qread(list_refsfiles[i])

  df <- dplyr::left_join(
    df,
    dafnee,
    by = c("oa_referenced_work_source_id" = "oa_source_id")
  )

  res <- df |>
    dplyr::group_by(oa_work_id, publisher_type, is_dafnee) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup()

  pos <- which(is.na(res$"publisher_type"))

  if (length(pos) > 0) {
    res[pos, "publisher_type"] <- "na_dafnee"
  }

  res <- res |>
    tidyr::pivot_wider(
      names_from = c("publisher_type", "is_dafnee"),
      values_from = "n",
      values_fill = 0
    )

  colnames(res) <- tolower(colnames(res))

  if (!("na_dafnee_na" %in% colnames(res))) {
    res$"na_dafnee_na" <- 0
  }

  if (!("fp_nonacademic" %in% colnames(res))) {
    res$"fp_nonacademic" <- 0
  }

  if (!("fp_academic" %in% colnames(res))) {
    res$"fp_academic" <- 0
  }

  if (!("np_academic" %in% colnames(res))) {
    res$"np_academic" <- 0
  }

  if (!("np_nonacademic" %in% colnames(res))) {
    res$"np_nonacademic" <- 0
  }

  res$"n_refs" <- rowSums(res[, -1])

  res$"oa_source_id" <- df$"oa_source_id"[1]

  res <- res[, c(
    "oa_source_id",
    "oa_work_id",
    "n_refs",
    "fp_nonacademic",
    "fp_academic",
    "np_academic",
    "np_nonacademic",
    "na_dafnee_na"
  )]

  res_list <- append(res_list, list(res))

  print(i)
}


## Export results ----

saveRDS(
  res_list,
  file = here::here("outputs", "number_of_cited_references_per_paper.rds")
)
