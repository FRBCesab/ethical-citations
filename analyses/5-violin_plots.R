#' Create a violin plot of proportion of FP/FP-academic/NP citations for the three publisher types
#'
#' Figure 2 of the article

## Import ratio per journal ----
ratios <- read.csv(here::here('outputs', 'mean_ratio_per_journal.csv'))

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
  here::here("figures", "SUPP_final_plot_3panel_priorfiltering.tiff"),
  combined_plot,
  width = 10,
  height = 4,
  dpi = 300,
  compression = "lzw"
)
