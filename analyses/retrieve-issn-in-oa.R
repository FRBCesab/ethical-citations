#' Retrieve ISSN for Daphnee Journals in OA database
#' 


## Read Daphnee database ----

daphnee <- read.csv(here::here("data", "raw_data", "DAFNEE_db_081024.csv"))


## Select fields ----

daphnee <- daphnee[daphnee$"Field" %in% c("ecology", "evolution/systematics", 
                                          "general"), ]


## Check for duplicates ----

any(duplicated(daphnee$"Journal"))


## Clean some Journal names ----

daphnee_journals <- data.frame("journal"       = daphnee$"Journal",
                               "journal_clean" = daphnee$"Journal")

daphnee_journals$"journal_clean" <- gsub("Proceedings of the National Academy of Sciences of the USA", 
                                         "Proceedings of the National Academy of Sciences", 
                                         daphnee_journals$"journal_clean")


## Prepare dataset ----

daphnee_journals$"oa_journal_name" <- NA
daphnee_journals$"oa_issn_l"       <- NA
daphnee_journals$"oa_issn"         <- NA


## Retrieve OA journal names and ISSN ----

for (i in 1:nrow(daphnee_journals)) {
  
  journal <- openalexR::oa_fetch(entity = "source",
                                 search = daphnee_journals[i, "journal_clean"]) |>
    as.data.frame()
  
  if (nrow(journal) > 0) {
    
    journal <- journal[which.max(journal$"relevance_score"), ]
    
    daphnee_journals[i, "oa_journal_name"] <- 
      journal[1, "display_name", drop = TRUE]
    daphnee_journals[i, "oa_issn"]         <- 
      paste0(unlist(journal[1, "issn", drop = TRUE]), collapse = " | ")
    daphnee_journals[i, "oa_issn_l"]       <- 
      journal[1, "issn_l", drop = TRUE]
  }
}


## Try to match by ISSN ----

impacts <- readxl::read_excel(here::here("data", "raw_data", 
                                         "CathleenPetit_JCR_JournalResults_10_2024.xlsx"), 
                              skip = 1)

issn <- unique(daphnee_journals$oa_issn_l)
issn <- issn[!is.na(issn)]

length(which(issn %in% impacts$ISSN))
