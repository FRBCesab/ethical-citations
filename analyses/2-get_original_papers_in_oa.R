#' Retrieve articles for DAFNEE Journals and for 2023
#' 


## Import DAFNEE journals ----

journals <- read.csv(here::here("data", "derived-data", "DAFNEE_db_with_impactact_Factor_filled.csv"))[,-1]

j_id <- journals$oa_source_id[1]



## Remove DAFNEE journals absent in OA ----

round(100 * sum(is.na(journals$"oa_works_count")) / nrow(journals), 0)

not_in_oa <- which(is.na(journals$"oa_works_count"))

journals <- journals[-not_in_oa,]

## Get articles for 2023 ----

# oa_get_original_papers()



## Are there works for a journal and for a particular year ----

openalexR::oa_fetch(entity           = "works", 
                    journal          = j_id,
                    publication_year = 2023, 
                    count_only       = TRUE)


## Retrieve works DOI for the journal and one specific year ----
## Limit is 200, need to loop
## if 200, then check if more on page 2

dois <- openalexR::oa_fetch(entity           = "works", 
                            journal          = j_id,
                            publication_year = 2023,
                            per_page         = 200,
                            pages            = NULL) |> 
  
  as.data.frame() |> 
  
  _[ , "doi", drop = TRUE]

# 21 dois for the 21 studies/works cited in the j_id of 2023



seq(1, counts, by = 200)




## Retrieve metadata for papers ----

doi <- c("10.1371/journal.pbio.3001640", "10.1038/s41597-023-02264-2")

metadata <- openalexR::oa_fetch(entity = "works", doi = dois) |> 
  as.data.frame()


## Count cited works per paper ----

lapply(metadata$"referenced_works", length) |> 
  unlist() 


## Extract cited works ----

references <- unlist(metadata$"referenced_works") # 883 for the 1st journal...


## Retrieve cited works metadata ----
## this is where things need to be looped on chunks

references <- openalexR::oa_fetch(entity = "works", identifier = references)


## Associate cited works w/ original papers ----

references <- lapply(1:length(dois), function(i) {
  
  cited_works <- data.frame("paper_doi" = dois[i],
                            "id"        = metadata$"referenced_works"[[i]])
  
  # merge(cited_works, references, by = "id", all = FALSE)
}) %>%
  do.call(rbind, .)

references <- do.call(rbind.data.frame, references)


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



