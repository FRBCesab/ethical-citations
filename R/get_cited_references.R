get_cited_references <- function(doi) {
  
  ## Check argument ----
  
  if (missing(doi)) {
    stop("Argument 'doi' is required")
  }
  
  if (!is.character(doi)) {
    stop("Argument 'doi' must be character")
  }
  
  if (length(doi) != 1) {
    stop("Argument 'doi' must be character of length 1")
  }
  
  
  ## Check if user is polite ----
  
  if (is.null(options()$"openalexR.mailto")) {
    stop("Be polite with OpenAlex API and run: ", 
         "`options(openalexR.mailto = 'your_email')`")
  }
  
  
  ## Prepare output ----
  
  cited_works <- data.frame("work_doi"      = doi,
                            "work_oaid"     = NA,
                            "n_cited_works" = NA)
  
  ## Retrieve work metadata from DOI ----
  
  metadata <- openalexR::oa_fetch(entity = "works", doi = doi) |> 
    as.data.frame()
  
  if (nrow(metadata) > 1) {
    stop("The doi '", doi, "' returns multiple works in OpenAlex database")
  }
  
  
  ## Count cited works and extract OA ID ----
  
  if (nrow(metadata) == 1) {
    cited_works$"work_oaid"     <- unlist(metadata$"id")
    cited_works$"n_cited_works" <- unlist(lapply(metadata$"referenced_works", 
                                                  length))
  }

  
  ## Retrieve cited works metadata ----
  
  cited_refs <- unlist(metadata$"referenced_works")
  
  references <- openalexR::oa_fetch(entity     = "works", 
                                    identifier = cited_refs,
                                    per_page   = 200,
                                    paging     = "page") |> 
    as.data.frame()
  
  
  ## Extract columns ----
  
  references <- references[ , c("id", "publication_year", "so", "doi")]
  colnames(references) <- c("cited_work_oaid", "cited_work_year", 
                            "cited_work_journal", "cited_work_doi")
  
  references$"work_doi" <- doi
  
  
  ## Clean output ----
  
  references <- merge(cited_works, references, by = "work_doi", all = TRUE)
  
  references
}
