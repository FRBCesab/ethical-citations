clean_string <- function(x){
  sapply(x, function(y){
    stringi::stri_trans_general(
      str = y, id = "Latin-ASCII") %>%
      tolower %>%
      gsub("^\\s+|\\s+$", "", .) %>%
      gsub("-|_|\\.", " ", .)
  })
}