# Camille: block 1; Nico: block 2.
# 1 fichier par journal ou tout le monde ensemble ? Ca va dépendre de la taille du fichier. Faut que ça passe sur GH.
# 

## Are there works for a journal and for a particular year ----

openalexR::oa_fetch(entity           = "works", 
                    journal          = journal_id,
                    publication_year = 2023, 
                    count_only       = TRUE) # count only for this query (this year, etc)


# if there are some matches, then do this:
## Retrieve works DOI for the journal and one specific year ----
## Limit is 200, need to loop on the number of pages, that need to be estimated. 

dois <- openalexR::oa_fetch(entity           = "works", 
                            journal          = journal_id,
                            publication_year = 2023,
                            per_page         = 200,
                            pages            = NULL) |> 
  
  as.data.frame() |> 
  
  _[ , "doi", drop = TRUE]
# loop et utiliser l'arg "page". seq(1, counts, by = 200)
# 
# # faut stocker tout ça dans un tab avec l'ID du journal, l'année, DOI. 2e fichier à exporter.



# 2. pour chaque DOI, choper les refs citées dans ces articles. Fairt article par article, ce sera plus généralisable (ShinyApp etc).
## Retrieve metadata for papers ----

doi <- c("10.1371/journal.pbio.3001640", "10.1038/s41597-023-02264-2")

metadata <- openalexR::oa_fetch(entity = "works", doi = doi) |> 
  as.data.frame()


## Count cited works per paper ----

lapply(metadata$"referenced_works", length) |> 
  unlist()


## Extract cited works ----

references <- unlist(metadata$"referenced_works")


## Retrieve cited works metadata ----

references <- openalexR::oa_fetch(entity = "works", identifier = references)


## Associate cited works w/ original papers ----

references <- lapply(1:length(doi), function(i) {
  
  cited_works <- data.frame("paper_doi" = doi[i],
                            "id"        = metadata$"referenced_works"[[i]])
  
  merge(cited_works, references, by = "id", all = FALSE)
})

references <- do.call(rbind.data.frame, references)