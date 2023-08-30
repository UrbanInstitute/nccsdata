#' Function to loop through and execute a vector of regex queries
#' on a vector of ntee codes
#' 
#' @description This

parse_ntee_regex <- function(regexp_vec, ntee_codes){
  matched_codes <- c()
  
  for (regexp in regexp_vec){
    results <- ntee_codes[grep(regexp, ntee_codes)]
    matched_codes <- c(results, matched_codes)
  }
  
  return(unique(matched_codes))
  
}