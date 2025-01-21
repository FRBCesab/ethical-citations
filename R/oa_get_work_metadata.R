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

oa_get_work_metadata <- function(work_id, mc_cores = 20) {
  
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
  
  
  ## Create batches ----
  
  batches <- seq(1, length(work_id), by = 200)
  batches <- sort(unique(c(batches, length(work_id) + 1)))
  
  
  articles <- data.frame()
  
  for (k in 1:(length(batches) - 1)) {
    
    ## Retrieve work metadata ----
  
    works <- suppressWarnings(
      suppressMessages(
      
        openalexR::oa_fetch(entity           = "works", 
                            identifier       = work_id[batches[k]:(batches[k + 1] - 1)],
                            per_page         = 200))) |> 
      
      as.data.frame()
  
  
    ## Clean work metadata ----
    
    articles <- rbind(articles, data.frame(
      "oa_referenced_work_id"        = works$"id",
      "oa_referenced_work_source_id" = works$"so_id"))

    Sys.sleep(sample(seq(0, 15, by = 1), 1))
    
  }
  
  articles <- articles[which(articles$"oa_referenced_work_id" %in% work_id), ]
  
  articles
}
