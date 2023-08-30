
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
source("01-data-preproc.R")

#' Function for data processing
#' This function takes the path to a .csv database of disaggregated 
#' and older ntee codes as an input
#' and creates a vector containing the population of new NTEE codes
#' The analyst can simply edit the csv to add or modify the population
#' of available codes


#' Function to get level 3 and 4 codes from 2 vectors containing
#' digits23 and digits45 respectively
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

#' Create function to return regex query for NTEE Codes
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

#' Function to loop through and execute a vector of regex queries
#' on a vector of ntee codes

parse_ntee_regex <- function(regexp_vec, ntee_codes){
  matched_codes <- c()
  
  for (regexp in regexp_vec){
    results <- ntee_codes[grep(regexp, ntee_codes)]
    matched_codes <- c(results, matched_codes)
  }
    
  return(unique(matched_codes))
  
}

#' Function to inspect user inputs and flag errors
validate_inp <- function(ntee.group,
                         ntee.code,
                         ntee.orgtype,
                         ind_group_codes,
                         level_2_4_codes,
                         org_type_codes){
  ifelse(
    ! ntee.group %in% c(ind_group_codes, "all"),
    stop("Invalid Industry Group \n 
          List of available groups can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md"),
    print("Collecting Matching Industry Groups")
  )
  
  ifelse(
    ! ntee.code %in% c(level_2_4_codes, "all"),
    ifelse(
      grepl("[A-Z][0-9xX]*[A-Z0-9xX]", ntee.code),
      print("Collecting Matching Industry Division and Subdivisions"),
      stop("Invalid Industry Division Subdivision Combination \n 
          List of available Combinations can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md")
    ),
    print("Collecting Matching Industry Division and Subdivisions")
      
  ) 
  
  
  ifelse(
    ! ntee.orgtype %in% c(org_type_codes, "all"),
    stop("Invalid Organization Type \n 
          List of available Organization Types can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md"),
    print("Collecting Matching Organization Types")
  )
    
}

#' Final function to return NTEE Codes from user inputs
parse_ntee <- function(ntee.group, ntee.code, ntee.orgtype){
  # Build dataset using disaggregated csv file
  ntee_code_ls = ntee_preproc()
  # Validate user inputs
  validate_inp(
    ntee.group = ntee.group,
    ntee.code = ntee.code,
    ntee.orgtype = ntee.orgtype,
    ind_group_codes = ntee_code_ls[[2]],
    level_2_4_codes = ntee_code_ls[[3]],
    org_type_codes = ntee_code_ls[[4]]
  )
  # Generate regex queries if inputs are valids
  regex_queries <- generate_ntee_regex(
    ntee.group = ntee.group,
    ntee.code = ntee.code,
    ntee.orgtype = ntee.orgtype
  )
  # Execute regex queries
  ntee2_codes <- parse_ntee_regex(
    regexp_vec = regex_queries,
    ntee_codes = ntee_code_ls[[1]]
  )
  # Return NTEE2 Codes
  return(list(ntee2_codes))
  
}
