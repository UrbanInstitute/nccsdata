
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
source("ntee_preproc.R")
source("get_ntee_level_3_4.R")

#' Function for data processing
#' This function takes the path to a .csv database of disaggregated 
#' and older ntee codes as an input
#' and creates a vector containing the population of new NTEE codes
#' The analyst can simply edit the csv to add or modify the population
#' of available codes

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
