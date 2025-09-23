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
  file = here::here("data", "raw-data", "list_of_selected_journals.csv")
)

dafnee <- dafnee[!duplicated(dafnee$"oa_source_name"), ]

pos <- which(dafnee$"business_model" == "University Press")

if (length(pos) > 0) {
  dafnee[pos, "business_model"] <- "Non-profit"
}


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
  gsub("https://openalex.org/", "", dafnee$"oa_source_id"),
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

  df <- df[which(df$"oa_source_id" != df$"oa_referenced_work_source_id"), ]  ####

  pos <- which(df$"business_model" == "For-profit")
  if (length(pos) > 0) {
    df[pos, "business_model"] <- "fp"
  }

  pos <- which(df$"business_model" == "Non-profit")
  if (length(pos) > 0) {
    df[pos, "business_model"] <- "np"
  }

  res <- df |>
    dplyr::group_by(oa_work_id, business_model, is_dafnee) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup()

  pos <- which(is.na(res$"business_model"))

  if (length(pos) > 0) {
    res[pos, "business_model"] <- "na_dafnee"
  }

  res <- res |>
    tidyr::pivot_wider(
      names_from = c("business_model", "is_dafnee"),
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

  colnames(res) <- gsub("^na_dafnee_na$", "na_dafnee", colnames(res))

  res_list <- append(res_list, list(res))

  print(i)
}


## Export results ----

saveRDS(
  res_list,
  file = here::here(
    "outputs",
    "number_of_cited_references_per_paper_wo_selfcitations.rds"
  )
)


#####
#####
#####


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
    "number_of_cited_references_per_paper_wo_selfcitations.rds"
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
  file = here::here("outputs", "mean_ratio_per_journal_wo_selfcitations.csv"),
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
  file = here::here("outputs", "mean_ratio_per_publisher_type_wo_selfcitations.csv"),
  row.names = FALSE
)



#####
#####
#####


## Import ratio per journal ----
ratios <- read.csv(here::here('outputs', 'mean_ratio_per_journal_wo_selfcitations.csv'))

#rename ref categories
names(ratios)[names(ratios) %in% c("fp_nonacademic", "fp_academic", "np")] <- c(
  "FP",
  "FP_acad",
  "NP"
)

#create single publisher type column, adapt labels and set order
ratios$publisher_type <- paste(
  ratios$business_model,
  ratios$is_academic,
  sep = '_'
)
ratios$publisher_type <- factor(
  ratios$publisher_type,
  levels = c('FP_FALSE', 'FP_TRUE', 'NP_TRUE')
)
ratios$publisher_type <- forcats::fct_recode(
  ratios$publisher_type,
  'FP' = 'FP_FALSE',
  'FP acad' = 'FP_TRUE',
  'NP' = 'NP_TRUE'
)

#One panel per type of reference that is cited, publisher type on x axis
#Containing stats (pairwise t-tests)m testing for sig differences in citation of i.e. NP among publisher types
vars <- c("FP", "FP_acad", "NP")


# Create list of plots
plot_list <- lapply(1:length(vars), function(i) {
  varname <- vars[i]

  # Perform Dunn’s test with Bonferroni correction
  dunn_res <- FSA::dunnTest(
    as.formula(paste(varname, "~ publisher_type")),
    data = ratios,
    method = "bonferroni"
  )

  #extract & format
  dunn_res_long <- dunn_res$res |>
    dplyr::rename(p.adj = P.adj) |>
    tidyr::separate(Comparison, into = c("group2", "group1"), sep = " - ") |>
    dplyr::mutate(
      y.position = 0.82, # adjust as needed per variable
      p.signif = dplyr::case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01 ~ "**",
        p.adj < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) |>
    dplyr::filter(p.signif != "ns") # only show significant comparisons

  #to draw y-axis only in first plot
  if (i != 1) {
    remove <- TRUE
  } else {
    remove <- FALSE
  }
  if (i == 1) {
    add <- TRUE
  } else {
    add <- FALSE
  }

  #to add panel label (a, b, c)
  label_layer <- switch(
    i,
    ggplot2::annotate(
      "text",
      x = 0.6,
      y = 1.05,
      label = "a) citing FP",
      fontface = "bold",
      size = 4,
      hjust = 0
    ),
    ggplot2::annotate(
      "text",
      x = 0.6,
      y = 1.05,
      label = "b) citing FP acad",
      fontface = "bold",
      size = 4,
      hjust = 0
    ),
    ggplot2::annotate(
      "text",
      x = 0.6,
      y = 1.05,
      label = "c) citing NP",
      fontface = "bold",
      size = 4,
      hjust = 0
    )
  )

  ggplot2::ggplot(
    ratios,
    ggplot2::aes_string(x = "publisher_type", y = varname)
  ) +
    ggplot2::geom_boxplot(fill = "white", width = 0.5, outlier.shape = NA) +
    #ylab(varname) +  # Change label dynamically if desired
    #xlab("Publisher Type") +
    ggdist::stat_halfeye(
      adjust = 0.5,
      width = 0.6,
      .width = 0,
      justification = -0.3,
      alpha = 0.3,
      fill = "#745392",
      point_colour = NA
    ) +
    ggplot2::geom_point(
      size = 1,
      alpha = 0.6,
      col = "#745392",
      fill = "#745392",
      position = ggplot2::position_jitter(seed = 1, width = 0.1)
    ) +

    ggplot2::geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      color = "darkgray",
      size = 1
    ) +
    ggpubr::stat_pvalue_manual(
      dunn_res_long,
      label = "p.signif",
      tip.length = 0.01,
      step.increase = 0.1,
      y.position = "y.position"
    ) +
    label_layer +
    ggplot2::scale_y_continuous(
      limits = c(-0.1, 1.05),
      breaks = seq(0, 1, by = 0.2)
    ) +
    #stat_n_text(size=3) + #added: to optionally add sample sizes (need to adapt scale_y_continuous lower limit to (i.e. -0.1))
    ggplot2::theme_minimal() + #added
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(linetype = "blank"),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5) #,
    ) +

    {
      if (remove) {
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank()
        )
      }
    } +

    {
      if (add) {
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 10)
        )
      }
    } +
    ggplot2::ggtitle('') #paste('Citing', varname, 'journals', sep=' ')
})

x_label <- grid::textGrob(
  "Publisher type",
  gp = grid::gpar(fontsize = 14)
)

y_label <- grid::textGrob(
  "Citation ratio (%)",
  gp = grid::gpar(fontsize = 14),
  rot = 90
)

combined_plot <- gridExtra::arrangeGrob(grobs = plot_list, ncol = 3)

combined_plot <- gridExtra::grid.arrange(
  gridExtra::arrangeGrob(
    y_label,
    combined_plot,
    ncol = 2,
    widths = grid::unit.c(
      grid::unit(1, "lines"),
      grid::unit(1, "npc") - grid::unit(1, "lines")
    )
  ),
  bottom = x_label
)

# Save to TIFF
ggplot2::ggsave(
  here::here("figures", "SUPP_final_plot_3panel_priorfiltering_wo_selfcitation.tiff"),
  combined_plot,
  width = 10,
  height = 4,
  dpi = 300,
  compression = "lzw"
)
