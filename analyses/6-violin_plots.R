library(ggplot2)

## Import ratio per journal ----

ratios <- read.csv(here::here("outputs", "mean_ratio_per_journal.csv"))


violin <- ratios |>

  ggplot(aes(
    x = publisher_type,
    y = np_mean,
    fill = publisher_type
  )) +

  geom_boxplot(alpha = 1, width = .48, outlier.shape = 21) +

  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    alpha = .6,
    point_colour = NA
  ) +

  EnvStats::stat_n_text(size = 3, y.pos = 1) +

  scale_fill_manual(values = c('salmon', 'slateblue')) +

  xlab('Publishing model') +
  ylab('Proportion of non-profit citations') +
  ylim(0, 1) +

  theme_bw() +
  theme(
    legend.position = 'none',
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line()
  )

ggsave(
  violin,
  file = here::here("figures", "figure_2-violinplots.png"),
  dpi = 300,
  width = 5.0,
  height = 3.5
)
