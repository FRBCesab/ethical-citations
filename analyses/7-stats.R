library(dplyr)
library(car)
library(FSA)

#STATS: Do different publisher_types cite significantly different proportions of reference_types?

#Check ANOVA assumptions
ratios_long<- ratios %>% pivot_longer(cols = FP:NP,
                                      names_to = "ref_type",
                                      values_to = "value") 
#normality per group
ratios_long %>%
  group_by(publisher_type, ref_type) %>%
  summarise(shapiro_p = shapiro.test(value)$p.value)
#=> not ok for NP publishers 

#QQ plots
ratios_long %>%
  ggplot(., aes(sample = value)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(ref_type ~ publisher_type)
#=> half looks ok...

#Homogeneity of variance
leveneTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP', ])
leveneTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP_acad', ])
leveneTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'NP', ])
#=> FP/FP_acad ok, p of NP significant -> not homogenous


#go for non-parametric:
kruskal.test(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP', ])
kruskal.test(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP_acad', ])
kruskal.test(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'NP', ])
#=> all sig =>

#post hoc: Dunn’s test
dunnTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP', ],
         method = "bonferroni")  # or method = "holm"
dunnTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP_acad', ],
         method = "bonferroni")  # or method = "holm"
dunnTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'NP', ],
         method = "bonferroni")  # or method = "holm"

#seems to match the indications in the plot (still try to properly implement)



#Updated plot
# Create list of plots
plot_list <- lapply(1:length(vars), function(i){
  varname <- vars[i]
  
  
  # Perform Dunn’s test with Bonferroni correction
  dunn_res <- dunnTest(
    as.formula(paste(varname, "~ publisher_type")),
    data = ratios,
    method = "bonferroni"
  )
  
  #extract & format
  dunn_res_long <- dunn_res$res %>%
    rename(p.adj = P.adj) %>%
    separate(Comparison, into = c("group2", "group1"), sep = " - ") %>%
    mutate(y.position = 0.82,  # adjust as needed per variable
           p.signif = case_when(
             p.adj < 0.001 ~ "***",
             p.adj < 0.01 ~ "**",
             p.adj < 0.05 ~ "*",
             TRUE ~ "ns"
           )) %>%
    filter(p.signif != "ns")  # only show significant comparisons
  
  
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
    stat_pvalue_manual(dunn_res_long,
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




