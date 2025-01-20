
#' Title
#' @description
#' This function counts the number of cited works for each original paper. A  to the original_paper dataframe.
#' 
#' @param orig_paper a `data.frame` from the file "/outputs/original_papers/"
#'
#' @return a `data.frame` with an extra column named `nb_citations` with the counts is added.
#' 
#' @export
#'
#' @examples
#' orig_pap <- qs::qread(here::here("outputs/original_papers/S102181007.qs"))
#' df <- oa_count_referenced_works(orig_pap)


oa_count_referenced_works <- function(orig_paper){
  
  orig_paper$nb_citations <- orig_paper$oa_referenced_work_id %>%
    lapply(., function(x){
      stringr::str_count(x, "\\|")
    } 
    )%>%
    unlist()
  orig_paper
}





