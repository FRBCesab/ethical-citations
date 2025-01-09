#' Get original articles of a journal for a specific year
#'
#' @description
#' This function requests the OpenAlex database by using the package 
#' `openalexR` and the function `oa_fetch()`. From a single journal and a 
#' specific year, it retrieves original articles metadata.
#'
#' @param journal_id a `character` of length 1. The OpenAlex identifier of the
#'   journal.
#'
#' @param year an `integer` of length 1. The publication year
#'
#' @return A `data.frame` with one row per original article and the following 
#' columns:
#'   - `oa_source_id`: the OpenAlex identifier of the journal
#'   - `oa_work_id`: the OpenAlex identifier of the original article
#'   - `oa_work_doi`: the DOI of the original article
#'   - `oa_work_year`: the publication year of the original article
#'   - `oa_referenced_work_id`: the OpenAlex identifier of the cited works 
#' 
#' @note
#' Before using this function, ensure that you ran: 
#' `options(openalexR.mailto = 'your_email')`.
#' 
#' @export
#'
#' @examples
#' ## Be polite ----
#' options(openalexR.mailto = 'user.email@mail.com')
#' 
#' ## Get original articles for one journal and one year ----
#' oa_get_original_papers(journal_id = "https://openalex.org/S2764636758",
#'                        year       = 2023)

oa_get_original_papers <- function(journal_id, year) {
  
  ## Check arguments ----
  
  if (missing(journal_id)) {
    stop("Argument 'journal_id' is required")
  }
  
  if (!is.character(journal_id)) {
    stop("Argument 'journal_id' must be character")
  }
  
  if (length(journal_id) != 1) {
    stop("Argument 'journal_id' must be character of length 1")
  }
  
  
  if (missing(year)) {
    stop("Argument 'year' is required")
  }
  
  if (!is.numeric(year)) {
    stop("Argument 'year' must be numeric")
  }
  
  if (length(year) != 1) {
    stop("Argument 'year' must be numeric of length 1")
  }
  
  
  ## Check if user is polite ----
  
  if (is.null(options()$"openalexR.mailto")) {
    stop("Be polite with OpenAlex API and run: ", 
         "`options(openalexR.mailto = 'your_email')`")
  }
  
  
  ## Init output ----
  
  articles <- data.frame()
  
  
  ## Get number of original articles ----
  
  counts <- openalexR::oa_fetch(entity           = "works", 
                                journal          = journal_id, 
                                publication_year = year, 
                                count_only       = TRUE) |> 
    
    as.data.frame()
  
  
  ## Retrieve original articles metadata ----
  
  if (counts$"count" > 0) {
    
    articles <- suppressMessages(
      
        openalexR::oa_fetch(entity           = "works", 
                            journal          = journal_id,
                            publication_year = year,
                            per_page         = 200,
                            paging           = "cursor")) |> 
          
          as.data.frame()
    
    
    ## Clean original articles metadata ----
    
    articles <- data.frame(
      "oa_source_id"             = journal_id,
      "oa_work_id"               = articles$"id",
      "oa_work_doi"              = articles$"doi",
      "oa_work_year"             = articles$"publication_year",
      "oa_referenced_work_id"    = list_to_str(articles$"referenced_works"))
    
    
    ## Remove duplicates ----
    
    articles <- articles[!duplicated(articles$"oa_work_id"), ]
  }
  
  articles
}
