#Caluclate ratios & count NAs of cited refs for each article ('oa_work_id')

#To Do:
#-loop through all list_refsfiles
#-save output
#(-calculate ratio per journal/category)

## List original article files ----
list_refsfiles <- list.files(path = here::here("outputs", "cited_references"), full.names = TRUE)

## Journal dafnee info to match ----
dafnee<- read.csv('data/derived-data/DAFNEE_db_with_impactact_Factor_filled.csv', header=T) #n=361
dafnee<- dafnee[, c("oa_source_id", "oa_source_name", "journal", "publisher_type", "business_model", "institution_type")]
dafnee<- dafnee[!is.na(dafnee$oa_source_id),] #n=341

#merge 'university press' into 'non-profit' --> DISCUSS/DECIDE
dafnee$publisher_type[dafnee$publisher_type=='University Press'] <- 'Non-profit'


#try with only one file
test <- qs::qread(list_refsfiles[1])

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



