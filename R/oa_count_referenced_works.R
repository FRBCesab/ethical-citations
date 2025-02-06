
#' Title
#' @description
#' This function counts the number of cited works for each original paper. A  to the original_paper dataframe.
#' 
#' @param original_paper a `data.frame` from the file "/outputs/original_papers/"
#' 
#' @param cited_references a `data.frame` from the file "/outputs/cited_references/"
#'
#' @return a `data.frame` with 4 columns. 
#' 
#' @export
#'
#' @examples
#' original_paper <- qs::qread(here::here("outputs/original_papers/S4078192.qs"))
#' cited_references <- qs::qread(here::here("outputs/cited_references/S4078192.qs"))
#' df <- oa_count_referenced_works(original_paper, cited_references)
#' cor(df$original_n_cited_works, df$final_n_cited_works)


oa_count_referenced_works <- function(original_paper, cited_references) {
  
  x <- original_paper$"oa_referenced_work_id" |>
    lapply(X = _, function(x) {
      stringr::str_count(x, "\\|") + 1
    }) |>
    unlist()

  x <- data.frame(
    oa_source_id           = original_paper$"oa_source_id",
    oa_work_id             = original_paper$"oa_work_id",
    original_n_cited_works = x
  )

  y <- tapply(
    X     = cited_references$"oa_referenced_work_id", 
    INDEX = cited_references$"oa_work_id", 
    FUN   = length)
  
  y <- data.frame(
    "oa_work_id" = rownames(y), 
    "final_n_cited_works" = y
  )

  merge(x, y, by = "oa_work_id", all = TRUE)

}
