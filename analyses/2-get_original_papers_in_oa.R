#' Retrieve articles for DAFNEE Journals and for 2023
#' 
#' Duration ~ 15 min


start <- Sys.time()


## Import DAFNEE journals ----

journals <- read.csv(here::here("data", "derived-data", 
                                "DAFNEE_db_with_issn.csv"))


## Remove DAFNEE journals absent in OA ----

journals <- journals[!is.na(journals$"oa_source_id"), ]


## Get original articles for 2023 ----

for (i in 1:nrow(journals)) {
  
  cat("Retrieving original papers for journal", i, "on", nrow(journals), "\r")
  
  articles <- oa_get_original_papers(journal_id = journals[i, "oa_source_id"], 
                                     year       = 2023)
  
  journal_id <- gsub("https://openalex.org/", "", journals[i, "oa_source_id"])
  
  qs::qsave(x    = articles,
            file = here::here("outputs", "original_papers", 
                              paste0(journal_id, ".qs")))
  
  # Sys.sleep(sample(1:5))
}


Sys.time() - start



## Retrieve metadata for original papers ----

# doi <- c("10.1371/journal.pbio.3001640", "10.1038/s41597-023-02264-2")

original_paper_metadata <- openalexR::oa_fetch(entity = "works", doi = dois) |> 
  as.data.frame() |>
  dplyr::select("id", "doi", "title", "author", "publication_year", "so", "so_id", "is_oa", "cited_by_count", "type","ab", "referenced_works")



# keep: author institution/country, abstract, institution type (inside the author df), 
# oa_status (is it open access or open alex?? check), type


 
## Count cited works per paper ----

lapply(original_paper_metadata$"referenced_works", length) |> 
  unlist() 


## Extract cited works ----

references <- unlist(original_paper_metadata$"referenced_works") # 883 for the 1st journal...


## Retrieve cited works metadata ----
## this is where things need to be looped on chunks

references <- openalexR::oa_fetch(entity = "works", identifier = references)


## Associate citations s w/ original papers ----

citations <- lapply(1:length(dois), function(i) {
  
  cited_works <- data.frame("orig_paper_doi" = dois[i],
                            "citation_id"        = original_paper_metadata$"referenced_works"[[i]])

  # merge(cited_works, references, by = "id", all = FALSE) 
}) %>%
  do.call(rbind, .)



## Extract citations_metadata : extracting the metadata for 50 citations takes ~30sec, and there are 854 for the 1st journal, and there are 341 journals
## lubridate::seconds_to_period(30*854*341) = "101d 2H 47M 0S"

  
  citation_metadata <- lapply(1:length(unique(citations$citation_id)), function(i){ 
    
    # oa id of the current citation
    cit <- unique(citations$citation_id)[i]
    
    # fetch metadata for that citation
    cit_meta <- openalexR::oa_fetch(entity = "works", cit) |> 
      as.data.frame()
    
    
    return(cit_meta)
    # thoughts:
    # keep: author institution/country, abstract, institution type (inside the author df), 
    # oa_status = open access or open alex?? didn't find out. check 
    
  }) %>%
    do.call(dplyr::bind_rows, .) %>%
    # this is the metadata we keep for each citation. Keep the Rshiny tool in mind.
    # the "author" field is a dataframe, it contains more information at the author level (e.g author insitution, etc)
    dplyr::select("id", "doi", "title", "author", "publication_year", "so", "so_id", "is_oa", "cited_by_count", "type","ab") %>%
    as.data.frame()

# rename column names to differentiate from the orig_journal_metadata column names. 
# Add "cit_" prefix:
colnames(citation_metadata) <- paste0("cit_", colnames(citation_metadata))


#################################################

# more thoughts:

# self citing: when journals preferentially cite works from their journal. 
# should we extract metadata to track this ?
# should we keep authors ?
# title.
# this extra info could be of use for the tool we want to develop.


# Camille: faire pour 1 journal.
# Export: 
# - 1 tab avec id du journal + doi des works cited
# - metadata des articles cités
# Nico automatise le reste.

# Partie de Nico



