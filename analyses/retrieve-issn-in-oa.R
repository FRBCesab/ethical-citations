#' Retrieve ISSN for Dafnee Journals in OA database
#' 


## Read Dafnee database ----

dafnee_journals <- read.csv(here::here("data", "raw-data", "DAFNEE-20241022.csv"))


## Select fields ----

dafnee_journals <- dafnee_journals[dafnee_journals$"Field" %in% c("ecology", "evolution/systematics", 
                                                                  "general", "organisms"), ]


## Check for duplicates ----

any(duplicated(dafnee_journals$"Journal"))


## Clean some Journal names ----

dafnee_journals$"journal_clean" <- dafnee_journals$"Journal"

dafnee_journals$"journal_clean" <- gsub("Proceedings of the National Academy of Sciences of the USA", 
                                        "Proceedings of the National Academy of Sciences", 
                                        dafnee_journals$"journal_clean")


## Prepare dataset ----

dafnee_journals$"oa_journal_name" <- NA
dafnee_journals$"oa_issn_l"       <- NA
dafnee_journals$"oa_issn"         <- NA
dafnee_journals$"oa_journal_id"   <- NA
dafnee_journals$"oa_n_papers"     <- NA


## Retrieve OA journal names and ISSN ----

for (i in 1:nrow(dafnee_journals)) {
  
  journal <- openalexR::oa_fetch(entity = "source",
                                 search = dafnee_journals[i, "journal_clean"]) |>
    as.data.frame()
  
  if (nrow(journal) > 0) {
    
    journal <- journal[which.max(journal$"relevance_score"), ]
    
    dafnee_journals[i, "oa_journal_name"] <- 
      journal[1, "display_name", drop = TRUE]
    
    dafnee_journals[i, "oa_issn"]         <- 
      paste0(unlist(journal[1, "issn", drop = TRUE]), collapse = " | ")
    
    dafnee_journals[i, "oa_issn_l"]       <- 
      journal[1, "issn_l", drop = TRUE]
    
    dafnee_journals[i, "oa_issn_l"]       <- 
      journal[1, "issn_l", drop = TRUE]
    
    dafnee_journals[i, "oa_n_papers"]     <- 
      journal[1, "works_count", drop = TRUE]
    
    dafnee_journals[i, "oa_journal_id"]     <- 
      journal[1, "id", drop = TRUE]
  }
}

round(100 * sum(is.na(dafnee_journals$oa_n_papers)) / nrow(dafnee_journals), 0)


## Add ISSN to Dafnee info ----

write.csv(dafnee_journals, here::here("data", "derived-data", "DAFNEE_db_with_issn.csv"), 
          row.names = FALSE)
