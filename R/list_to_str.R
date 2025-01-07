list_to_str <- function(x) {
  
  if (is.null(x)) {
    
    return(NA)
    
  } else {
    
    x <- unlist(lapply(x, function(y) paste0(y, collapse = " | ")))
    
    pos <- which(x == "")
    
    if (length(pos)) x[pos] <- NA
  }
  
  x
}
