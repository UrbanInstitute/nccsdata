
#' A complete function takes any user input values from the arguments:

#'    ntee.group
#'    ntee.code
#'    ntee.orgtype

#' And returns the correct list of NTEE codes that match the filter 
#' requirements.

#' It will also raise an error if the user tries argument values that are 
#' undefined and print an informative message.
#' 
#' Load packages
library("stringr")
library("data.table")

#' Read csv file to extract different versions of NTEE Codes

ntee_disagg_df <- read.csv("ntee-disaggregated.csv")
ntee2_level1 <- ntee_disagg_df$broad.category
ntee2_level2 <- ntee_disagg_df$major.group

#' Extract digits23 and digits 45
digits23 <- substring(ntee_disagg_df$old.code, 2, 3)
digits45 <- substring(ntee_disagg_df$old.code, 4, 5)
digits45 <- replace(digits45, digits45 == "", "00")


#' Use digits23 and digits45 to get level 3 and 4 of new NTEE code
get_ntee_level_3_4 <- function(digits23, digits45){
  
  ntee2_level_3_4 <- ifelse(
    as.numeric(digits23) > 19,
    substring(digits23, 1, 2),
    substring(digits45, 1, 2)
  )

  ntee2_level_3_4 <- ifelse(
    is.na(ntee2_level_3_4),
    substring(digits23, 1, 2),
    ntee2_level_3_4
  )
  
  return(ntee2_level_3_4) 
}

ntee2_level_3_4 <- mapply(get_ntee_level_3_4, digits23, digits45)

#' Extract level 5 code from disaggregated csv
ntee2_level5 <- ntee_disagg_df$type.org

#' Create universe of applicable codes
ntee_new_codes <- paste(
  ntee2_level1,
  "-",
  ntee2_level2,
  ntee2_level_3_4,
  "-",
  ntee2_level5,
  sep = ""
)

#' Create function to return regex query for NTEE Codes
generate_ntee_regex <- function(ntee.group = "", ntee.code = "", ntee.orgtype = ""){
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

#' Function to loop through and execute a vector of regex queries
#' on a vector of ntee codes

parse_ntee_regex <- function(regexp_vec, ntee_codes){
  matched_codes <- c()
  
  for (regexp in regexp_vec){
    results <- ntee_codes[grep(regexp, ntee_codes)]
    matched_codes <- c(results, matched_codes)
  }
    
  return(matched_codes)
  
}


