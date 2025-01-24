# For each article ('oa_work_id') calculate:
#- number of refs not in dafnee (i.e. giving 'NA')
#- # total references
#- # for-profit (fp) and non-profit (np) journals
#- proportion of fp (np) journals considering those that are in dafnee ('prop_np', 'prop_fp')
#- ratio np/fp (not even sure if that makes sense???)

#For each journal ('res_journallvl'):
#- mean prop_np and prop_fp
#- # articles


#To Do:
#- left_join df + dafnee gives many-to-many warning -> check
#- properly save article-level tables (here as res_list)
#- match journal name to journal-level table for easier interpretation


library(dplyr)
## List original article files ----
list_refsfiles <- list.files(path = here::here("outputs", "cited_references"), full.names = TRUE)

## Journal dafnee info to match ----
dafnee<- read.csv('data/derived-data/DAFNEE_db_with_impactact_Factor_filled.csv', header=T) #n=361
dafnee<- dafnee[, c("oa_source_id", "oa_source_name", "journal", "publisher_type", "business_model", "institution_type")]
dafnee<- dafnee[!is.na(dafnee$oa_source_id),] #n=341

#merge 'university press' into 'non-profit' --> DISCUSS/DECIDE
dafnee$publisher_type[dafnee$publisher_type=='University Press'] <- 'Non-profit'


res_list<- list() #to save table per journal
res_journallvl<-matrix(nrow=0, ncol=4) #to save journal-level ratios
colnames(res_journallvl)<- c('oa_source_id', 'prop_np', 'prop_fp', 'n_articles')

for (i in 1:20) { #length(list_refsfiles)
  #read file
  df <- qs::qread(list_refsfiles[i])
  
  #bind dafnee table
  df<- dplyr::left_join(df, dafnee, 
                        by=c("oa_referenced_work_source_id"="oa_source_id"))
  
  res<- df %>% group_by(oa_work_id, publisher_type) %>% summarise(n=n()) %>% ungroup()
  res<- res %>% tidyr::pivot_wider(names_from='publisher_type', values_from='n', values_fill=0) %>%
    rename('fp'='For-profit', 'np'='Non-profit', 'na_dafnee'='NA') #rename because colnames are not ideal, mabe change earlier for 'dafnee'
  res$np_fp<- round(res$np/res$fp, 4)
  res$n_refs<- rowSums(res[ , c('np', 'fp', 'na_dafnee')]) 
  
  #of those refs in dafnee, how many are np (fp)?
  res$prop_np<- round(res$np/(res$np+res$fp), 4)
  res$prop_fp<- 1-res$prop_np #round(res$fp/(res$np+res$fp), 4)
  
  #add journal id, set uniform order of columns
  res$oa_source_id<- df$oa_source_id[1]
  res<- res[, c('oa_source_id', 'oa_work_id', 'n_refs', 'na_dafnee', 'np', 'fp', 'np_fp', 'prop_np', 'prop_fp')]
  
  #calculate journal mean ratio
  j_prop_np<- round(mean(res$prop_np[is.finite(res$prop_np)]), 4)
  j_prop_fp<- round(mean(res$prop_fp[is.finite(res$prop_fp)]), 4)
  
  n_articles<- nrow(res)
  
  #bind results
  res_list<- append(res_list, list(res))
  res_journallvl <- rbind(res_journallvl, c(df$oa_source_id[1], j_prop_np, j_prop_fp, n_articles))
  
}

#save
save(res_list, file='outputs/ratios_articlelevel.R')
write.csv(res_journallvl, file='outputs/ratios_journallevel.csv')






### to delete ###
#to save results
res<-matrix(nrow=0, ncol=7)
colnames(res)<- c('oa_source_id', 'oa_work_id', 'n_refs', 'na_refs', 'np', 'fp', 'np_fp')

for (i in unique(test$oa_work_id)) {

  df0<- test[test$oa_work_id== i, ]
  
  #bind dafnee table
  df<- dplyr::left_join(df0, dafnee, 
                        by=c("oa_referenced_work_source_id"="oa_source_id"))
  
  #calculate ratio(s) & counts NAs
  fp <- length(which(df$publisher_type=='For-profit')) #for-profit
  np <- length(which(df$publisher_type=='Non-profit')) #non-profit
  np_fp<- round(np/fp, 5)
  
  #maybe we want to add business model as well for curiosity?
  
  n_refs<- nrow(df)
  nas<- sum(is.na(df$journal))
  
  #bind ratios/values
  res <- rbind(res, c(df$oa_source_id[1], i, n_refs, nas, np, fp, np_fp))
}



