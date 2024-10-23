#' Get cited references of one single article
#'
#' @description
#' This function requests the OpenAlex database by using the package 
#' `openalexR` and the function `oa_fetch()`. From a single DOI, it retrieves
#' cited references of the original work.
#'
#' @param doi a `character` of length 1. The DOI of the paper for which cited 
#'   references must be extracted.
#'
#' @return A `data.frame` with the following columns:
#'   - `work_doi`: the DOI provided by the user, i.e. the original paper
#'   - `work_oa_id`: the OpenAlex identifier of the original paper
#'   - `n_cited_works`: the total number of cited references (indexed in OA)
#'   - `cited_work_oa_id`: the OpenAlex identifier of the cited reference
#'   - `cited_work_year`: the publication year of the cited reference
#'   - `cited_work_source`: the source of the cited reference
#'   - `cited_work_doi`: the DOI of the cited reference
#' 
#' @note
#' Before using this function, ensure that you ran: 
#' `options(openalexR.mailto = 'your_email')`.
#' 
#' @export
#'
#' @examples
#' ## Be polite ----
#' options(openalexR.mailto = 'rdev.nc@gmail.com')
#' 
#' ## Get cited references for one work (article) ----
#' get_cited_references(doi = "10.1371/journal.pbio.3001640")

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
                            "work_oa_id"    = NA,
                            "n_cited_works" = NA)
  
  
  ## Retrieve work metadata from DOI ----
  
  metadata <- openalexR::oa_fetch(entity = "works", doi = doi) |> 
    as.data.frame()
  
  if (nrow(metadata) > 1) {
    stop("The DOI '", doi, "' returns multiple works in OpenAlex database")
  }
  
  
  ## Count cited works and extract OA ID ----
  
  if (nrow(metadata) == 1) {
    cited_works$"work_oa_id"    <- unlist(metadata$"id")
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
  
  colnames(references) <- c("cited_work_oa_id", "cited_work_year", 
                            "cited_work_source", "cited_work_doi")
  
  references$"work_doi" <- doi
  
  
  ## Clean output ----
  
  references <- merge(cited_works, references, by = "work_doi", all = TRUE)
  references <- references[!duplicated(references$"cited_work_oa_id"), ]
  
  rownames(references) <- NULL
  
  references
}
