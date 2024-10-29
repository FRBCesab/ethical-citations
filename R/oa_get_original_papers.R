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
  
  
  ## ... ----
  
}
