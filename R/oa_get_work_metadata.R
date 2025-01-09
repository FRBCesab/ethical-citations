#' Get cited references metadata
#'
#' @description
#' This function requests the OpenAlex database by using the package 
#' `openalexR` and the function `oa_fetch()`. From one or several works (cited 
#' references), it retrieves associated metadata (journal identifier).
#'
#' @param work_id a `character` vector. The OpenAlex identifier of the articles
#'   (cited references).
#'
#' @return A `data.frame` with one row per cited reference and the following 
#' columns:
#'   - `oa_referenced_work_id`: the OpenAlex identifier of the cited reference
#'   - `oa_referenced_work_source_id`: the OpenAlex identifier of the source of
#'      the cited reference
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
#' ## Get cited references for one work (article) ----
#' oa_get_work_metadata(work_id = c("https://openalex.org/W788673684",
#'                                  "https://openalex.org/W898153436"))

oa_get_work_metadata <- function(work_id) {
  
  ## Check argument ----
  
  if (missing(work_id)) {
    stop("Argument 'work_id' is required")
  }
  
  if (!is.character(work_id)) {
    stop("Argument 'work_id' must be character")
  }
  
  
  ## Check if user is polite ----
  
  if (is.null(options()$"openalexR.mailto")) {
    stop("Be polite with OpenAlex API and run: ", 
         "`options(openalexR.mailto = 'your_email')`")
  }
  
  
  ## Remove missing values ----
  
  work_id <- work_id[!is.na(work_id)]
  
  
  ## Remove duplicates ----
  
  work_id <- work_id[!duplicated(work_id)]
  
  
  ## Retrieve work metadata ----
  
  works <- suppressWarnings(
    suppressMessages(
    
      openalexR::oa_fetch(entity           = "works", 
                          identifier       = work_id,
                          per_page         = 200,
                          paging           = "cursor"))) |> 
    
    as.data.frame()
  
  
  ## Clean work metadata ----
  
  articles <- data.frame(
    "oa_referenced_work_id"        = works$"id",
    "oa_referenced_work_source_id" = works$"so_id")
  
  articles <- articles[which(articles$"oa_referenced_work_id" %in% work_id), ]
  
  articles
}
