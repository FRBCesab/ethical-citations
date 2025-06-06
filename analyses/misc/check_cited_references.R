# Check cited references

journals  <- list.files(here::here("outputs", "original_papers"))
journals <- gsub("\\.qs", "", journals)

original_papers  <- list.files(here::here("outputs", "original_papers"), full.names = TRUE)
cited_references <- list.files(here::here("outputs", "cited_references"), full.names = TRUE)

thecorrs <- NULL

for (i in 1:length(original_papers)) {

  cat("Processing journal", i, "on", length(original_papers), "\r")

  if (length(grep(journals[i], cited_references)) == 1) {
    
    original_paper  <- qs::qread(here::here("outputs", "original_papers", paste0(journals[i], ".qs")))
    cited_reference <- qs::qread(here::here("outputs", "cited_references", paste0(journals[i], ".qs")))
  
    tmp <- oa_count_referenced_works(original_paper, cited_reference)
    thecorrs <- c(thecorrs, cor(tmp$"original_n_cited_works", tmp$"final_n_cited_works"))
  }
}

plot(thecorrs, pch = 21, ylim = c(-1, 1))
