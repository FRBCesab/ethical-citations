#'
#' Create a violin plot of proportion of FP/FP-academic/NP citations for the three publisher types
#'
#' Figure 2 of the article
#'
library(dplyr)
library(ggplot2)
library(ggdist)
library(ggpubr)
library(gridExtra)  
library(grid)
library(EnvStats)

## Import ratio per journal ----
ratios <- read.csv(here::here('outputs','mean_ratio_per_journal.csv'))

#rename ref categories
names(ratios)[names(ratios) %in% c("fp_nonacademic", "fp_academic", "np")] <- c("FP", "FP_acad", "NP")

#create single publisher type column, adapt labels and set order
ratios$publisher_type<- paste(ratios$business_model, ratios$is_academic, sep='_')
ratios$publisher_type<- factor(ratios$publisher_type, levels=c('FP_FALSE', 'FP_TRUE', 'NP_TRUE'))
ratios$publisher_type<- fct_recode(ratios$publisher_type, 'FP'='FP_FALSE', 'FP acad'='FP_TRUE', 'NP'='NP_TRUE')

#One panel per type of reference that is cited, publisher type on x axis
#Containing stats (pairwise t-tests)m testing for sig differences in citation of i.e. NP among publisher types
vars <- c("FP", "FP_acad", "NP")

# Create list of plots
plot_list <- lapply(1:length(vars), function(i){
  varname <- vars[i]
  
  # Compute ANOVA
  anova_result <- aov(as.formula(paste(varname, "~ publisher_type")), data = ratios)
  
  # Perform pairwise t-tests with Bonferroni correction & prepare for plotting
  pairwise <- pairwise.t.test(
    ratios[[varname]],
    ratios$publisher_type,
    p.adjust.method = "bonferroni")
  
  bonf_df <- as.data.frame(pairwise$p.value)
  bonf_df$group1 <- rownames(bonf_df)
  bonf_df_long <- tidyr::pivot_longer(bonf_df,
                                      cols = -group1,
                                      names_to = "group2",
                                      values_to = "p.adj") %>%
    dplyr::filter(!is.na(p.adj)) %>%
    dplyr::mutate(y.position = 0.82, #prev: 0.95 but was hiding third bracket
                  p.signif = dplyr::case_when(p.adj < 0.001 ~ "***",
                                              p.adj < 0.01 ~ "**",
                                              p.adj < 0.05 ~ "*",
                                              TRUE ~ "ns")) %>%
    filter(p.signif != "ns") #added (to have only significant differences shown)
  
  #to draw y-axis only in first plot
  if (i != 1) {remove=T
  }else{remove=F}
  if (i == 1) {add=T
  }else{add=F}
  
  #to add panel label (a, b, c)
  label_layer <- switch(i, annotate("text", x = 0.6, y = 1.05, label = "a) citing FP", fontface = "bold", size = 4, hjust = 0),
                        annotate("text", x = 0.6, y = 1.05, label = "b) citing FP acad", fontface = "bold", size = 4, hjust = 0),
                        annotate("text", x = 0.6, y = 1.05, label = "c) citing NP", fontface = "bold", size = 4, hjust = 0))
  
  ggplot(ratios, aes_string(x = "publisher_type", y = varname)) +
    geom_boxplot(fill = "white", width = 0.5, outlier.shape = NA) +
    #ylab(varname) +  # Change label dynamically if desired
    #xlab("Publisher Type") +
    ggdist::stat_halfeye(adjust = 0.5,
                         width = 0.6,
                         .width = 0,
                         justification = -0.3,
                         alpha = 0.3,
                         fill = "#745392",
                         point_colour = NA) +
    geom_point(size = 1,
               alpha = 0.6,
               col = "#745392", fill="#745392",
               position = position_jitter(seed = 1, width = 0.1)) +
    geom_hline(yintercept = 0.5,
               linetype = "dashed",
               color = "darkgray",
               size = 1) +
    stat_pvalue_manual(bonf_df_long,
                       label = "p.signif",
                       tip.length = 0.01,
                       step.increase = 0.1,
                       y.position = "y.position") +
    label_layer +
    scale_y_continuous(limits = c(-0.1, 1.05),
                       breaks = seq(0, 1, by = 0.2)) +
    #stat_n_text(size=3) + #added: to optionally add sample sizes (need to adapt scale_y_continuous lower limit to (i.e. -0.1))
    theme_minimal() + #added
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.line.x = element_line(linetype = "blank"),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title = element_text(hjust = 0.5)#,
          #axis.ticks.y = element_blank()
    ) +
    {if(remove)theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())} +
    {if(add)theme(axis.text.y = element_text(size = 10)#,
                  #axis.line.y = element_line(linetype = "solid", colour = "black")
    )} +
    ggtitle('') #paste('Citing', varname, 'journals', sep=' ')
})

x_label <- textGrob("Publisher type", gp=gpar(fontsize=14))
y_label <- textGrob("Citation ratio (%)", gp=gpar(fontsize=14), rot=90)

combined_plot <- arrangeGrob(grobs = plot_list, ncol = 3)
combined_plot<- grid.arrange(arrangeGrob(y_label, combined_plot, ncol = 2,
                                         widths = unit.c(unit(1, "lines"), unit(1, "npc") - unit(1, "lines"))),
                             bottom = x_label)

# Save to TIFF
ggsave(here::here("figures","SUPP_final_plot_3panel_priorfiltering.tiff"), combined_plot,
       width = 10, height = 4, dpi = 300, compression = "lzw")



#_________________________________________________
#initial plot using facets (keep for the moment)
#plot journal level
library(EnvStats)
p<-ratio_per_journals %>% 
  pivot_longer(fp_nonacademic:np, names_to='cat', values_to='value') %>%
  mutate(journal_cat = paste(business_model, is_academic, sep='_'),
         journal_cat = recode(journal_cat, 'FP_FALSE' = 'FP_only', 
                              'FP_TRUE' = 'FP_academic',
                              'NP_TRUE' = 'NP'),
         journal_cat = factor(journal_cat, levels=c('FP_only', 'FP_academic', 'NP')),
         cat = factor(cat, levels=c('fp_nonacademic', 'fp_academic', 'np')),
         cat = recode(cat, 'fp_nonacademic' = 'Refs FP_only',
                      'fp_academic' = 'Refs FP_academic',
                      'np' = 'Refs NP')) %>%
  #filter(cat == 'NP') %>% #add for single type plot
  ggplot(aes(x=journal_cat, y=value)) + #, fill=journal_cat
  #geom_violin(fill='lightgray', color = NA) +
  geom_boxplot(alpha=.6, width= .48, outlier.shape=21, outlier.alpha = 0, fill='slateblue') + #, fill='#dddddd'
  ggdist::stat_halfeye(adjust= .5,
                       width= .6,
                       .width= 0,
                       alpha= 0.3, point_colour=NA,
                       justification = -0.3,
                       fill='slateblue'
  ) +
  geom_point(size = 1, alpha = 0.2,
             col = "slateblue", position = position_jitter(seed = 1, width = 0.1)
  ) + 
  geom_hline(yintercept = 0.5, linetype = "dashed",
             color = "darkgray", size = 1) +
  facet_wrap(~cat) + #remove for single type plot
  #scale_fill_manual(values=c('#745392', '#909278', '#b0c700')) +#'#898584'  # '#c3c4b5' '#909278' CESAB colors
  #scale_fill_manual(values=c('#777777', '#777777', '#777777')) +#for single type plot
  xlab('Publisher type') + ylab('Citation ratio') + #Citation of NP refs (%)
  ylim(0,1) +
  #ggtitle('not filtering for limit of 5, based on all refs') +
  theme_minimal() +
  stat_n_text(size=3) +
  theme(legend.position='bottom', #'none' for single type plot
        panel.grid.major.y=element_blank(), 
        panel.border=element_blank(),
        #axis.text.x=element_blank(), #remove for single type plot
        axis.line.x=element_line(), axis.line.y=element_line()) #+
p
ggsave(p, file='figures/final_journallevel_violinbox_all.jpg', dpi=200, width=6, height=3)
ggsave(p, file='figures/final_journallevel_violinbox_np.png', dpi=200, width=3.5, height=3.5)
