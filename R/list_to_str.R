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


str_to_list <- function(data, column, split = " \\| ") {
  
  strings <- strsplit(data[ , column], split = split)
  
  new_data <- data.frame()
  
  for (i in 1:length(strings)) {
    
    tmp <- data.frame(data[i, -which(colnames(data) == column)], strings[[i]], 
                      row.names = NULL)
    
    colnames(tmp)[ncol(tmp)] <- column
    
    new_data <- rbind(new_data, tmp)
  }
  
  pos <- which(new_data[ , column] == "NA")
  
  if (length(pos) > 0) {
    new_data[pos, column] <- NA
  }
  
  new_data
}
