#' Function to loop through and execute a vector of regex queries
#' on a vector of ntee codes
#' 
#' @description This function executes a vector of regex queries on 
#' a vector containing the population of NTEE2 codes
#' 
#' @param regexp_vec character vector. Vector containing all regex queries
#' @param ntee_codes character vector. Vector containing population of NTEE2
#' codes
#' 
#' @usage parse_ntee_regex(regexp_vec, ntee_codes)
#' 
#' @return vector of matched NTEE2 codes
#' 
#' @export

parse_ntee_regex <- function(regexp_vec, ntee_codes){
  
  matched_codes <- c()
  
  for (regexp in regexp_vec){
    results <- ntee_codes[grep(regexp, ntee_codes)]
    matched_codes <- c(results, matched_codes)
  }
  
  return(unique(matched_codes))
  
}
