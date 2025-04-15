# For each article ('oa_work_id') calculate:
#- number of refs not in dafnee (i.e. giving 'NA')
#- # total references
#- # count for-profit (fp) and non-profit (np) journals
#- proportion of fp (np) journals considering those that are in dafnee and identified as either np/fp ('prop_np_found', 'prop_fp_found') and overall ('prop_np', 'prop_fp)

#For each journal:
#- mean & sd prop_np and prop_fp
#- # articles

library(dplyr)

## List original article files ----
list_refsfiles <- list.files(path = here::here("outputs", "cited_references"), full.names = TRUE)

## Journal dafnee info to match ----
dafnee<- read.csv('data/derived-data/DAFNEE_db_with_issn.csv', header=T) #n=361
dafnee<- dafnee[, c("oa_source_id", "oa_source_name", "journal", "publisher_type", "business_model", "institution_type")]
dafnee<- dafnee[!is.na(dafnee$oa_source_id),] #n=341 # CC: I get n=337 here

#merge 'university press' into 'non-profit' --> DISCUSS/DECIDE
dafnee$publisher_type[dafnee$publisher_type=='University Press'] <- 'Non-profit'
dafnee<- dafnee %>% mutate(publisher_type=case_when(publisher_type=='For-profit' ~ 'fp',
                                                   publisher_type=='Non-profit' ~ 'np'))



res_list<- list() #to save table per journal
#res_journallvl<-matrix(nrow=0, ncol = 12) #to save journal-level ratios
#colnames(res_journallvl)<- c('oa_source_id', 'n_articles', 'mean_prop_np', 
#                             'mean_prop_fp','mean_prop_nadafnee', 
#                             'mean_prop_np_found', 'mean_prop_fp_found',
#                             'sd_prop_np', 'sd_prop_fp', 'sd_prop_nadafnee',
#                             'sd_prop_np_found', 'sd_prop_fp_found')


for (i in 1:length(list_refsfiles)) { #1:length(list_refsfiles)
  #read file
  df <- qs::qread(list_refsfiles[i])
  
  #bind dafnee table
  df<- dplyr::left_join(df, dafnee, 
                        by=c("oa_referenced_work_source_id"="oa_source_id"))
  
  res<- df %>% 
    group_by(oa_work_id, publisher_type) %>% 
    summarise(n = n()) %>% 
    ungroup()

  res$publisher_type[is.na(res$publisher_type)] <- "na_dafnee"
  res<- res %>% 
    tidyr::pivot_wider(names_from='publisher_type', values_from='n', values_fill=0)
  

  if(!('fp' %in% names(res))){
    res$fp <- 0
  }
  if(!('np' %in% names(res))){
    res$np <- 0
  }
  if(!('na_dafnee' %in% names(res))){
    res$na_dafnee <- 0
  }
  #make sure all columns are present (might miss if one category is absent)
  #res<- res %>% mutate(fp = ifelse("fp" %in% names(.), fp, NA),
  #                     np = ifelse("np" %in% names(.), np, NA),
  #                     na_dafnee = ifelse("na_dafnee" %in% names(.), na_dafnee, NA)) 
  
  #res$np_fp<- round(res$np/res$fp, 4) #NOT INTERESSTING
  res$n_refs<- rowSums(res[ , c('np', 'fp', 'na_dafnee')]) 
  
  #of those refs in dafnee, how many are np (fp)?
  res$prop_np<- round(res$np/res$n_refs, 4)
  res$prop_fp<- round(res$fp/res$n_refs, 4)
  res$prop_nadafnee <- round(res$na_dafnee/res$n_refs, 4)
  res$prop_np_found <- round(res$np/(res$fp + res$np), 4)
  res$prop_fp_found <- round(res$fp/(res$fp + res$np), 4)

  res$prop_np_found <- ifelse(is.na(res$prop_np_found), 0, res$prop_np_found)
  res$prop_fp_found <- ifelse(is.na(res$prop_fp_found), 0, res$prop_fp_found)
  
  #add journal id, set uniform order of columns
  res$oa_source_id<- df$oa_source_id[1]
  
  
  res<- res[, c('oa_source_id', 'oa_work_id', 'n_refs', 'na_dafnee', 'np', 'fp',
                'prop_np', 'prop_fp', 'prop_np_found', 'prop_fp_found', 
                'prop_nadafnee')]
  
  #bind results
  res_list<- append(res_list, list(res))
  
  #calculate JOURNAL mean ratios
  #j_prop_np<- round(mean(res$prop_np[is.finite(res$prop_np)]), 4)
  #j_prop_fp<- round(mean(res$prop_fp[is.finite(res$prop_fp)]), 4)
  #j_prop_nadafnee<- round(mean(res$prop_nadafnee[is.finite(res$prop_nadafnee)]), 4)
  #
  #j_prop_np_found <- round(mean(res$prop_np_found[is.finite(res$prop_np_found)]), 4)
  #j_prop_fp_found <- round(mean(res$prop_fp_found[is.finite(res$prop_fp_found)]), 4)
  #  
  #j_prop_np_sd<- round(sd(res$prop_np[is.finite(res$prop_np)]), 4)
  #j_prop_fp_sd<- round(sd(res$prop_fp[is.finite(res$prop_fp)]), 4)
  #j_prop_nadafnee_sd<- round(sd(res$prop_nadafnee[is.finite(res$prop_nadafnee)]), 4)
  #
  #j_prop_np_sd_found <- round(sd(res$prop_np_found[is.finite(res$prop_np_found)]), 4)
  #j_prop_fp_sd_found <- round(sd(res$prop_fp_found[is.finite(res$prop_fp_found)]), 4)
  
  #n_articles<- nrow(res)

  #fd <- data.frame(
  #  'oa_source_id' = df$oa_source_id[1],
  #  'n_articles' = n_articles,
  #  'mean_prop_np' = j_prop_np,  
  #  'mean_prop_fp' = j_prop_fp,
  #  'mean_prop_nadafnee' = j_prop_nadafnee ,  
  #  'mean_prop_np_found' = j_prop_np_found,
  #  'mean_prop_fp_found' = j_prop_fp_found,
  #  'sd_prop_np' = j_prop_np_sd,
  #  'sd_prop_fp' = j_prop_fp_sd,
  #  'sd_prop_nadafnee' = j_prop_nadafnee_sd,
  #  'sd_prop_np_found' = j_prop_np_sd_found,
  #  'sd_prop_fp_found' = j_prop_fp_sd_found
  #)

  #res_journallvl <- rbind(res_journallvl, fd)
  print(i)
}

#save
saveRDS(res_list, file='outputs/ratios_articlelevel_unfiltered.rds')
#write.csv(res_journallvl, file='outputs/ratios_journallevel_unfiltered.csv')


## Aggregate by journal ----

x <- readRDS('outputs/ratios_articlelevel_unfiltered.rds')
x <- data.frame(do.call(rbind.data.frame, x))

#remove articles citing < 5 refs
n_ref_min <- 5 #loosing n=7433
x <- x[x$n_refs > n_ref_min, ]

#make sure still XX articles to be considered in ratio when excluding na_dafnee
#--> ! loosing half of dataset but hardly changing anything??
x <- x[(x$fp+x$np) > n_ref_min, ]

xx <- x |> 
  group_by(oa_source_id) |>
  summarise(across(c(prop_np:prop_nadafnee), list(mean=mean, sd=sd), na.rm=TRUE)) |> 
  mutate(across(where(is.numeric), round, 4)) |>
  ungroup()

#merge 'university press' into 'non-profit' --> DISCUSS/DECIDE
# on first look it was same direction of ratios as non-profit (np > fp)
#add journal name & dafnee status
dafnee<- read.csv('data/derived-data/DAFNEE_db_with_issn.csv', header=T)
xx<- left_join(xx, dafnee, by='oa_source_id')
xx$publisher_type[xx$publisher_type=='University Press'] <- 'Non-profit'
write.csv(xx, file='outputs/ratios_journallevel_filtered_min5ref_min5found.csv', row.names=F)

#plots
xx |> pivot_longer(contains('_mean'), names_to = 'ratio', values_to = 'value') |>
  mutate(ratio= str_sub(ratio, end=-6)) |>
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_grid(~ factor(ratio, levels=c('prop_np','prop_fp','prop_nadafnee','prop_np_found','prop_fp_found'))) +
  theme_bw() +
  ggtitle('journal-level filtered(5refs)')

xx |> 
  filter(!is.na(publisher_type)) |> 
  pivot_longer(contains('found_mean'), names_to = 'ratio', values_to = 'value') |>
  mutate(ratio= str_sub(ratio, end=-6)) |>#,
        # ratio= factor(ratio, levels=c('prop_np','prop_fp','prop_nadafnee','prop_np_found','prop_fp_found'))) |>
  ggplot(aes(x=ratio, y=value)) +
  geom_violin(fill='palegreen', color = NA) +
  geom_boxplot(alpha=0.2) +
  theme_bw() +
  facet_grid(~publisher_type) +
  ggtitle('journal-level filtered(5refs)')

#final figure 3:
library(grid)
library(gridExtra)
p1<- xx |> 
  pivot_longer(contains('found_mean'), names_to = 'ratio', values_to = 'value') |>
  mutate(ratio= str_sub(ratio, end=-12)) |>
  ggplot(aes(x=ratio, y=value)) +
  geom_violin(fill='lightgray', color = NA) +
  geom_boxplot(color='#666666', alpha=0.2) +
  stat_summary(fun="mean",
               geom="point",
               colour="red") +
  #geom_point(y=mean(value)) +
  theme_bw() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x=NULL, y=NULL, subtitle='Overall')

p2<- xx |> filter(publisher_type=='Non-profit') |> 
  pivot_longer(contains('found_mean'), names_to = 'ratio', values_to = 'value') |>
  mutate(ratio= str_sub(ratio, end=-12)) |>
  ggplot(aes(x=ratio, y=value)) +
  geom_violin(fill='lightgray', color = NA) +
  geom_boxplot(color='#666666', alpha=0.2) +
  stat_summary(fun="mean",
               geom="point",
               colour="red") +
  theme_bw() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x=NULL, y=NULL, subtitle='Non-profit')

p3<- xx |> filter(publisher_type=='For-profit') |> 
  pivot_longer(contains('found_mean'), names_to = 'ratio', values_to = 'value') |>
  mutate(ratio= str_sub(ratio, end=-12)) |>
  ggplot(aes(x=ratio, y=value)) +
  geom_violin(fill='lightgray', color = NA) +
  geom_boxplot(color='#666666', alpha=0.2) +
  stat_summary(fun="mean",
               geom="point",
               colour="red") +
  theme_bw() +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(x=NULL, y=NULL, subtitle='For-profit')

ytext <- textGrob('Percent',rot=90, gp=gpar(fontsize=12))
xtext<- textGrob('Publisher Type', gp=gpar(fontsize=12))
combined_plot<-grid.arrange(p1, p2, p3, nrow=1, bottom=xtext, left=ytext) ## display plot
ggsave(file='figures/violinbox_journallevel_split_filtered_min5ref.png', combined_plot) 


#final figure (NP only)
p<- xx |> 
  filter(!is.na(publisher_type)) |> 
  ggplot(aes(x=publisher_type, y=prop_np_found_mean)) +
  geom_violin(fill='lightgray', color = NA) +
  geom_boxplot(alpha=0.2) +
  xlab('Journals publisher type') + ylab('Proportion NP citations') +
  theme_bw() +
  ggtitle('journal-level filtered(5refs)')
ggsave(file='../figures/violinbox_journallevel_violin_NPonly.png', p) 


#quick T test
np<- xx[xx$publisher_type=='Non-profit', "prop_np_found_mean"]
fp<- xx[xx$publisher_type=='For-profit', "prop_np_found_mean"]
t.test(np, fp, alternative=c("two.sided"))

#check conditions ()
#normality:
shapiro.test(xx$prop_np_found_mean[xx$publisher_type %in% c('np', 'fp')])



## Aggregate by category level  ----
xx<- read.csv('outputs/ratios_journallevel_filtered_min5ref.csv')

xx |> group_by(publisher_type) |>
  summarise(across(contains('mean'), mean, na.rm=TRUE)) |> 
  mutate(across(where(is.numeric), round, 4)) |>
  ungroup()

xx |> 
  pivot_longer(contains('mean'), names_to = 'ratio', values_to = 'value') |>
  mutate(ratio= str_sub(ratio, end=-6),
         ratio= factor(ratio, levels=c('prop_np','prop_fp','prop_nadafnee','prop_np_found','prop_fp_found'))) |>
  ggplot(aes(x=ratio, y=value)) +
  geom_violin(fill='seashell', color = NA) +
  geom_boxplot(alpha=0.2) +
  theme_bw() +
  facet_grid(~publisher_type)  |>
  ggtitle('journal-level filtered(5refs)')


###
###
###

# Compute ratio (Nico alternative) ----

## Import citations ----

citations <- readRDS(here::here("outputs", "ratios_articlelevel_unfiltered.rds"))
citations <- do.call(rbind, citations)


## Add Dafnee journal information ----

citations <- merge(citations, dafnee, by = "oa_source_id", all = FALSE)


## Remove original papers w/ less than 5 citations (total) ----

citations <- citations[citations$"n_refs" > 5, ]


## Compute NP ratios ----

np_journals <- citations[citations$"publisher_type" == "np", ]

np_ratio <- np_journals$"np" / np_journals$"n_refs"
mean(np_ratio)


## Compute FP ratios ----

fp_journals <- citations[citations$"publisher_type" == "fp", ]

fp_ratio <- fp_journals$"fp" / fp_journals$"n_refs"
mean(fp_ratio)


## Remove original papers w/ no Dafnee citations (keep only found) ----

citations_daf <- citations[which((citations$"n_refs" - citations$"na_dafnee") > 0), ]


## Compute NP ratios (found) ----

np_journals_daf <- citations_daf[citations_daf$"publisher_type" == "np", ]

np_ratio_daf <- np_journals_daf$"np" / (np_journals_daf$"fp" + np_journals_daf$"np")
mean(np_ratio_daf)


## Compute FP ratios (found) ----

fp_journals_daf <- citations_daf[citations_daf$"publisher_type" == "fp", ]

fp_ratio_daf <- fp_journals_daf$"fp" / (fp_journals_daf$"fp" + fp_journals_daf$"np")
mean(fp_ratio_daf)
