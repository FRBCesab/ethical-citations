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
      "oa_referenced_works"      = list_to_str(articles$"referenced_works"))
    
    
    ## Remove duplicates ----
    
    articles <- articles[!duplicated(articles$"oa_work_id"), ]
  }
  
  articles
}
