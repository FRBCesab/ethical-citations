#'
#' Compute NP and FP citations ratios per journal and per publisher type
#'

## Import Dafnee info ----

dafnee <- read.csv(
  file = here::here("data", "derived-data", "final_list_of_journals.csv")
)


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

citations <- citations[(citations$"n_refs" - citations$"na_dafnee_na") > 5, ] #losing: 40326 refs

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
SDs$var<- 'sd'

all<- rbind(ratios, SDs)

write.csv(
  x = ratios,
  file = here::here("outputs", "mean_ratio_per_publisher_type.csv"),
  row.names = FALSE
)

#plot journal level
library(EnvStats)
p<-ratio_per_journals %>% 
  pivot_longer(fp_nonacademic:np, names_to='cat', values_to='value') %>%
  mutate(journal_cat = paste(business_model, is_academic, sep='_'),
         journal_cat = recode(journal_cat, 'FP_FALSE' = 'FP non-academic', 
                              'FP_TRUE' = 'FP academic',
                              'NP_TRUE' = 'NP'),
         journal_cat = factor(journal_cat, levels=c('FP non-academic', 'FP academic', 'NP')),
         cat = factor(cat, levels=c('fp_nonacademic', 'fp_academic', 'np')),
         cat = recode(cat, 'fp_nonacademic' = 'FP non-academic',
                      'fp_academic' = 'FP academic',
                      'np' = 'NP')) %>%
  filter(cat == 'NP') %>% #add for single type plot
  ggplot(aes(x=journal_cat, y=value)) + #, fill=journal_cat
  #geom_violin(fill='lightgray', color = NA) +
  geom_boxplot(alpha=.6, width= .48, outlier.shape=21, outlier.alpha = 0, fill='#dddddd') +
  ggdist::stat_halfeye(adjust= .5,
                       width= .6,
                       .width= 0,
                       alpha= 1, point_colour=NA,
                       fill='#888888'
  ) +
  #facet_wrap(~cat) + #remove for single type plot
  #scale_fill_manual(values=c('#745392', '#909278', '#b0c700')) +#'#898584'  # '#c3c4b5' '#909278'
  #scale_fill_manual(values=c('#777777', '#777777', '#777777')) +#for single type plot
  xlab('Publisher type') + ylab('Citation of NP refs (%)') +
  ylim(0,1) +
  theme_bw() +
  stat_n_text(size=3) +
  theme(legend.position='none', #'none' for single type plot
        panel.grid.major.y=element_blank(), 
        panel.border=element_blank(),
        #axis.text.x=element_blank(), #remove for single type plot
        axis.line.x=element_line(), axis.line.y=element_line()) #+
p
ggsave(p, file='figures/final_journallevel_violinbox_all.png', dpi=200, width=6, height=3.5)
ggsave(p, file='figures/final_journallevel_violinbox_np.png', dpi=200, width=3.5, height=3.5)


#ANOVA/Kruskal-Wallis
ratio_per_journals$journal_cat<- paste(ratio_per_journals$business_model, ratio_per_journals$is_academic, sep='_')
kruskal.test( ~ journal_cat, data = ratio_per_journals)
pairwise.wilcox.test(ratio_per_journals$np, ratio_per_journals$journal_cat,
                     p.adjust.method = "bonferroni") #same result for 'BH'

#sig: NP citations between all three journal types
#sig: FP-academic citations between FP_academic-FP_nonacademic, FP_academic-NP
#sig: FP-nonacademic citations between FP_nonacademic-FP_academic, FP_nonacademic-NP

#final counts for MM/Sup (can be deleted in the end)
#(to be run before each step of filtering)
citations %>% mutate(journal_cat = paste(publisher_type, is_dafnee, sep='_')) %>%
  mutate(nref_included=n_refs-na_dafnee_na) %>%
  #group_by(journal_cat) %>%
  summarise(n_articles=n(),
            n_refs=sum(nref_included))

n_distinct(citations$oa_source_name)

#number of articles per journal
citations %>% mutate(journal_cat = paste(publisher_type, is_dafnee, sep='_')) %>%
  mutate(nref_included=n_refs-na_dafnee_na) %>%
  group_by(oa_source_name) %>%
  summarise(n_articles=n(),
            n_refs=sum(nref_included)) %>% ungroup() %>%
  summarise(mean_articles=mean(n_articles),
            med_articles=median(n_articles),
            sd_articles=sd(n_articles))

#number of refs per article
citations %>% mutate(journal_cat = paste(publisher_type, is_dafnee, sep='_')) %>%
  mutate(nref_included=n_refs-na_dafnee_na) %>%
  summarise(mean_refs=mean(nref_included),
            med_refs=median(nref_included),
            sd_refs=sd(nref_included))


#plot mean/sd
ratios %>% 
  pivot_longer(fp_nonacademic:np, names_to='cat', values_to='value') %>%
  mutate(journal_cat = paste(business_model, is_academic, sep='_'),
         cat= factor(cat, levels=c('fp_nonacademic', 'fp_academic', 'np'))) %>%
  ggplot(., aes(x=journal_cat, y=value)) +
  geom_point() +
  facet_wrap(~cat) +
  theme_bw()




# ratio_per_journals <- rbind(
#   ratio_per_journals,
#   data.frame(
#     "publisher_type" = "Non-profit",
#     "oa_source_id" = names(np_np_ratio),
#     "np_mean" = round(np_np_ratio, 2),
#     "fp_mean" = 1 - round(np_np_ratio, 2)
#   )
# )

# ratios[1, "np_mean"] <- round(mean(np_np_ratio), 2)
# ratios[1, "np_sd"] <- round(sd(np_np_ratio), 2)

# ## Compute FP ratios for NP Journals ----

# fp_ratio <- np_journals$"fp" / (np_journals$"np" + np_journals$"fp")

# np_fp_ratio <- data.frame("oa_source_id" = np_journals$"oa_source_id", fp_ratio)
# np_fp_ratio <- tapply(
#   np_fp_ratio$"fp_ratio",
#   INDEX = np_fp_ratio$"oa_source_id",
#   mean
# )

# ratios[1, "fp_mean"] <- round(mean(np_fp_ratio), 2)
# ratios[1, "fp_sd"] <- round(sd(np_fp_ratio), 2)

# ## Compute FP ratios for FP Journals----

# fp_journals <- citations[citations$"publisher_type" == "For-profit", ]

# fp_ratio <- fp_journals$"fp" / (fp_journals$"np" + fp_journals$"fp")

# fp_fp_ratio <- data.frame("oa_source_id" = fp_journals$"oa_source_id", fp_ratio)
# fp_fp_ratio <- tapply(
#   fp_fp_ratio$"fp_ratio",
#   INDEX = fp_fp_ratio$"oa_source_id",
#   mean
# )

# ratio_per_journals <- rbind(
#   ratio_per_journals,
#   data.frame(
#     "publisher_type" = "For-profit",
#     "oa_source_id" = names(fp_fp_ratio),
#     "np_mean" = 1 - round(fp_fp_ratio, 2),
#     "fp_mean" = round(fp_fp_ratio, 2)
#   )
# )

# ratios[2, "fp_mean"] <- round(mean(fp_fp_ratio), 2)
# ratios[2, "fp_sd"] <- round(sd(fp_fp_ratio), 2)

# ## Compute NP ratios for FP Journals----

# np_ratio <- fp_journals$"np" / (fp_journals$"np" + fp_journals$"fp")

# fp_np_ratio <- data.frame("oa_source_id" = fp_journals$"oa_source_id", np_ratio)
# fp_np_ratio <- tapply(
#   fp_np_ratio$"np_ratio",
#   INDEX = fp_np_ratio$"oa_source_id",
#   mean
# )

# ratios[2, "np_mean"] <- round(mean(fp_np_ratio), 2)
# ratios[2, "np_sd"] <- round(sd(fp_np_ratio), 2)

# rownames(ratio_per_journals) <- NULL

## Export results ----

write.csv(
  x = ratio_per_journals,
  file = here::here("outputs", "mean_ratio_per_journal.csv"),
  row.names = FALSE
)


write.csv(
  x = ratios,
  file = here::here("outputs", "mean_ratio_per_publisher_type.csv"),
  row.names = FALSE
)
