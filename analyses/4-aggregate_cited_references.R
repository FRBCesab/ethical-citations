#'
#' Compute number of citations per article
#'
#' In particular:
#' - n_refs: total number of citations indexed in OA database
#' - np: number of citations of non-profit journals
#' - fp: number of citations of for-profit journals
#' - na_dafnee: number of citations of journals missing in Dafnee database

## List cited reference files ----

list_refsfiles <- list.files(
  path = here::here("outputs", "cited_references"),
  full.names = TRUE
)

## Import Dafnee info ----

dafnee <- read.csv(
  file = here::here("data", "derived-data", "DAFNEE_db_with_issn.csv")
)


## Clean Dafnee info ----

dafnee <- dafnee[, c(
  "oa_source_id",
  "oa_source_name",
  "journal",
  "publisher_type",
  "business_model",
  "institution_type"
)]

dafnee <- dafnee[!is.na(dafnee$"oa_source_id"), ]

pos <- which(dafnee$"publisher_type" == "University Press")
if (length(pos) > 0) {
  dafnee[pos, "publisher_type"] <- "Non-profit"
}

dafnee <- dafnee |>
  dplyr::mutate(
    publisher_type = dplyr::case_when(
      publisher_type == 'For-profit' ~ 'fp',
      publisher_type == 'Non-profit' ~ 'np'
    )
  )


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
    dplyr::group_by(oa_work_id, publisher_type) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup()

  pos <- which(is.na(res$"publisher_type"))

  if (length(pos) > 0) {
    res[pos, "publisher_type"] <- "na_dafnee"
  }

  res <- res |>
    tidyr::pivot_wider(
      names_from = "publisher_type",
      values_from = "n",
      values_fill = 0
    )

  if (!("fp" %in% names(res))) {
    res$"fp" <- 0
  }

  if (!("np" %in% names(res))) {
    res$"np" <- 0
  }

  if (!("na_dafnee" %in% names(res))) {
    res$"na_dafnee" <- 0
  }

  res$"n_refs" <- rowSums(res[, c("np", "fp", "na_dafnee")])

  res$"oa_source_id" <- df$"oa_source_id"[1]

  res <- res[, c(
    "oa_source_id",
    "oa_work_id",
    "n_refs",
    "np",
    "fp",
    "na_dafnee"
  )]

  res_list <- append(res_list, list(res))

  print(i)
}


## Export results ----

saveRDS(
  res_list,
  file = here::here("outputs", "number_of_cited_references_per_paper.rds")
)
