#STATS: Do different publisher_types cite significantly different proportions of reference_types?



## Import ratio per journal ----
ratios <- read.csv(here::here('outputs','mean_ratio_per_journal.csv'))
#rename ref categories
names(ratios)[names(ratios) %in% c("fp_nonacademic", "fp_academic", "np")] <- c("FP", "FP_acad", "NP")

#One panel per type of reference that is cited, publisher type on x axis
#Containing stats (pairwise t-tests)m testing for sig differences in citation of i.e. NP among publisher types
vars <- c("FP", "FP_acad", "NP")

#create single publisher type column, adapt labels and set order
ratios$publisher_type<- paste(ratios$business_model, ratios$is_academic, sep='_')
ratios$publisher_type<- factor(ratios$publisher_type, levels=c('FP_FALSE', 'FP_TRUE', 'NP_TRUE'))
ratios$publisher_type<- forcats::fct_recode(ratios$publisher_type, 'FP'='FP_FALSE', 'FP acad'='FP_TRUE', 'NP'='NP_TRUE')

#Check ANOVA assumptions
ratios_long<- ratios |> tidyr::pivot_longer(cols = FP:NP,
                                      names_to = "ref_type",
                                      values_to = "value") 
#normality per group
ratios_long |>
  dplyr::group_by(publisher_type, ref_type) |>
  dplyr::summarise(shapiro_p = shapiro.test(value)$p.value)
#=> not ok for NP publishers 

# CC: shapiro test's null hypothesis is that the data are normally distributed:
# - if pval < 0.05, the null is rejected, and the the data are NOT normal
# - if pval >= 0.05, null cannot be rejected and data are considerened normally distributed

# In addition, the data aren't independant since the sum of values for a given journal = 1. But grouping by journal would lead to groups of n=3



#QQ plots
ratios_long |>
  ggplot2::ggplot(ggplot2::aes(sample = value)) +
  ggplot2::stat_qq() + ggplot2::stat_qq_line() +
  ggplot2::facet_wrap(ref_type ~ publisher_type)
#=> half looks ok...

#Homogeneity of variance
car::leveneTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP', ])
car::leveneTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP_acad', ])
car::leveneTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'NP', ])
#=> FP/FP_acad ok, p of NP significant -> not homogenous


#go for non-parametric:
kruskal.test(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP', ])
kruskal.test(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP_acad', ])
kruskal.test(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'NP', ])
#=> all sig =>

#post hoc: Dunn’s test
FSA::dunnTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP', ],
         method = "bonferroni")  # or method = "holm"
FSA::dunnTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'FP_acad', ],
         method = "bonferroni")  # or method = "holm"
FSA::dunnTest(value ~ publisher_type, data=ratios_long[ratios_long$ref_type == 'NP', ],
         method = "bonferroni")  # or method = "holm"

#seems to match the indications in the plot (still try to properly implement)




