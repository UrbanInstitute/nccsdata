#' Script to generate regex queries for parsing population of NTEE2 codes.

generate_ntee_regex <- function(ntee.group, ntee.code, ntee.orgtype){
  # Formulate regex query based on user input
  
  level1_query <- ifelse(
    ntee.group == "all",
    "[A-Z][A-Z][A-Z]",
    ntee.group
  )
  
  level_2to4_query <- ifelse(
    ntee.code == "all",
    "[A-Z][0-9][A-Z0-9]",
    
    ifelse(
      grepl("[A-Z]$", ntee.code) | grepl("[A-Z][xX][xX]$", ntee.code),
      paste(substring(ntee.code, 1, 1), "[0-9][A-Z0-9]", sep = ""),
      
      ifelse(
        grepl("[A-Z][0-9]$", ntee.code) | grepl("[A-Z][0-9][xX]$", ntee.code),
        paste(substring(ntee.code, 1, 2), "[A-Z0-9]", sep = ""),
        ntee.code
      )
    )
  )
  
  level_5_query <- ifelse(
    ntee.orgtype == "all",
    "[A-Z][A-Z]",
    ntee.orgtype
  )
  
  full_query <- paste(
    level1_query,
    "-",
    level_2to4_query,
    "-",
    level_5_query,
    sep = "")
  
  
  full_query <- data.table::CJ(
    level1_query,
    level_2to4_query,
    level_5_query
  )[, paste(level1_query,
            level_2to4_query, 
            level_5_query,
            sep = "-")]
  
  return(full_query)
}