#' Retrieve cited references metadata for original papers
#' 

options(timeout = 3600)


## List original article files ----

original_articles_files <- list.files(path       = here::here("outputs", "original_papers"),
                                      full.names = TRUE)


## Get cited references ----

for (i in 1:length(original_articles_files)) {

  cat("Retrieving cited references for journal", i, "on", 
      length(original_articles_files), "\r")

  original_articles <- qs::qread(original_articles_files[i])
  
  if (nrow(original_articles) > 0) {
    
    original_articles <- str_to_list(data   = original_articles, 
                                     column = "oa_referenced_work_id")
    
    referenced_works <- oa_get_work_metadata(
      work_id  = original_articles$"oa_referenced_work_id",
      mc_cores = 20)
    
    referenced_works <- merge(original_articles, referenced_works, 
                              by = "oa_referenced_work_id", all = TRUE)
    
    referenced_works <- referenced_works[ , c(2:5, 1, 6)]
    
    journal_id <- gsub("https://openalex.org/", "", 
                       referenced_works[1, "oa_source_id"])
    
    qs::qsave(x    = referenced_works,
              file = here::here("outputs", "cited_references", 
                                paste0(journal_id, ".qs"))) 
  }
}
