# For each article ('oa_work_id') calculate:
#- number of refs not in dafnee (i.e. giving 'NA')
#- # total references
#- # for-profit (fp) and non-profit (np) journals
#- proportion of fp (np) journals considering those that are in dafnee ('prop_np', 'prop_fp')
#- ratio np/fp (not even sure if that makes sense???)

#For each journal ('res_journallvl'):
#- mean & sd prop_np and prop_fp
#- # articles


#To Do:
#- left_join df + dafnee gives (sometimes...) many-to-many warning -> check!
#- properly save article-level tables (here as res_list)

library(dplyr)

## List original article files ----
list_refsfiles <- list.files(path = here::here("outputs", "cited_references"), full.names = TRUE)

## Journal dafnee info to match ----
dafnee<- read.csv('data/derived-data/DAFNEE_db_with_issn.csv', header=T) #n=361
dafnee<- dafnee[, c("oa_source_id", "oa_source_name", "journal", "publisher_type", "business_model", "institution_type")]
dafnee<- dafnee[!is.na(dafnee$oa_source_id),] #n=341

#merge 'university press' into 'non-profit' --> DISCUSS/DECIDE
dafnee$publisher_type[dafnee$publisher_type=='University Press'] <- 'Non-profit'
dafnee<- dafnee %>% mutate(publisher_type=case_when(publisher_type=='For-profit' ~ 'fp',
                                                   publisher_type=='Non-profit' ~ 'np'))



res_list<- list() #to save table per journal
res_journallvl<-matrix(nrow=0, ncol = 12) #to save journal-level ratios
colnames(res_journallvl)<- c('oa_source_id', 'n_articles', 'mean_prop_np', 
                             'mean_prop_fp','mean_prop_nadafnee', 
                             'mean_prop_np_found', 'mean_prop_fp_found',
                             'sd_prop_np', 'sd_prop_fp', 'sd_prop_nadafnee',
                             'sd_prop_np_found', 'sd_prop_fp_found')


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
    res$fp <- NA
  }
  if(!('np' %in% names(res))){
    res$np <- NA
  }
  if(!('na_dafnee' %in% names(res))){
    res$na_dafnee <- NA
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

  #calculate JOURNAL mean ratios
  j_prop_np<- round(mean(res$prop_np[is.finite(res$prop_np)]), 4)
  j_prop_fp<- round(mean(res$prop_fp[is.finite(res$prop_fp)]), 4)
  j_prop_nadafnee<- round(mean(res$prop_nadafnee[is.finite(res$prop_nadafnee)]), 4)
  
  j_prop_np_found <- round(mean(res$prop_np_found[is.finite(res$prop_np_found)]), 4)
  j_prop_fp_found <- round(mean(res$prop_fp_found[is.finite(res$prop_fp_found)]), 4)
  
  j_prop_np_sd<- round(sd(res$prop_np[is.finite(res$prop_np)]), 4)
  j_prop_fp_sd<- round(sd(res$prop_fp[is.finite(res$prop_fp)]), 4)
  j_prop_nadafnee_sd<- round(sd(res$prop_nadafnee[is.finite(res$prop_nadafnee)]), 4)
  
  j_prop_np_sd_found <- round(sd(res$prop_np_found[is.finite(res$prop_np_found)]), 4)
  j_prop_fp_sd_found <- round(sd(res$prop_fp_found[is.finite(res$prop_fp_found)]), 4)
  
  
  n_articles<- nrow(res)
  
  #bind results
  res_list<- append(res_list, list(res))

  fd <- data.frame(
    'oa_source_id' = df$oa_source_id[1],
    'n_articles' = n_articles,
    'mean_prop_np' = j_prop_np,  
    'mean_prop_fp' = j_prop_fp,
    'mean_prop_nadafnee' = j_prop_nadafnee ,  
    'mean_prop_np_found' = j_prop_np_found,
    'mean_prop_fp_found' = j_prop_fp_found,
    'sd_prop_np' = j_prop_np_sd,
    'sd_prop_fp' = j_prop_fp_sd,
    'sd_prop_nadafnee' = j_prop_nadafnee_sd,
    'sd_prop_np_found' = j_prop_np_sd_found,
    'sd_prop_fp_found' = j_prop_fp_sd_found
  )

  res_journallvl <- rbind(res_journallvl, fd)
  print(i)
}


#save
saveRDS(res_list, file='outputs/ratios_articlelevel_unfilteredraw.rds')
write.csv(res_journallvl, file='outputs/ratios_journallevel_unfilteredraw.csv')


x <- readRDS('outputs/ratios_articlelevel_unfilteredraw.rds')
x <- do.call(rbind.data.frame, x)

n_ref_min <- quantile(na.omit(x$n_refs), 0.2) ##########

x <- x[x$n_refs > n_ref_min, ]


## Aggregate by journal ----

xxx <- x |> 
  group_by(oa_source_id) |> 
  summarise(
    mean_prop_np  = mean("prop_np", na.rm = TRUE),
    prop_fp       = mean("prop_fp", na.rm = TRUE),
    prop_nadafnee = mean("prop_nadafnee", na.rm = TRUE),
    prop_np_found = mean("prop_np_found", na.rm = TRUE),
    prop_fp_found = mean("prop_fp_found", na.rm = TRUE)) |> 
  ungroup()



##### STOP


#add journal name & dafnee status
res_journallvl<- data.frame(res_journallvl)
res_journallvl<- left_join(res_journallvl, dafnee, by='oa_source_id')
write.csv(res_journallvl, file='outputs/ratios_journallevel.csv')


#quick view: results on publisher_type_level
sum(is.na(res_journallvl$publisher_type)) #n=15 

res_journallvl %>% 
  group_by(publisher_type) %>% 
  summarise(
    mean_propnp_found = mean(as.numeric(mean_prop_np_found), na.rm = TRUE), 
    mean_propfp_found = mean(as.numeric(mean_prop_fp_found), na.rm = TRUE)
  )

res_journallvl %>% 
  group_by(publisher_type) %>% 
  summarise(
    mean_propnp = mean(as.numeric(mean_prop_np), na.rm = TRUE), 
    mean_propfp = mean(as.numeric(mean_prop_fp), na.rm = TRUE)
  )