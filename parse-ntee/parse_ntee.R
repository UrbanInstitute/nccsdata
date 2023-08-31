
#' A complete function takes any user input values from the arguments:

#'    ntee.group
#'    ntee.code
#'    ntee.orgtype

#' And returns the correct list of NTEE codes that match the filter 
#' requirements.

#' It will also raise an error if the user tries argument values that are 
#' undefined and print an informative message.
#' 
#' Load required packages in folder
source("ntee_preproc.R")
source("get_ntee_level_3_4.R")
source("generate_ntee_regex.R")
source("parse_ntee_regex.R")
source("validate_inp.R")

#' Function that generates population of NTEE2 codes and filters codes that
#' match user inputs
#' 
#' @description This function takes user defined Industry Group, Industry, 
#' Division, Subdivision and Organization Types and filters population of NTEE2
#' codes to find codes that match user inputs.
#' 
#' @param ntee.group character vector. Vector of desired Industry Group codes
#'  to filter. Use "all" to include all possible codes.
#' @param ntee.code character vector. Sequence of desired Industry, Division and
#' Subdivision codes (old code structure) to use in filtering. Use "all" to 
#' include all possible codes. Can also provide only partial codes. For example
#' "A" or "Axx" will query NTEE2 codes based on Industry group "A" and all
#' division and subdivisions.
#' @param ntee.orgtype character vector. Vector of Organization Types.
#' Use "all" to include all possible codes.
#' 
#' @usage parse_ntee(ntee.group, ntee.code, ntee.orgtype)
#' 
#' @returns list of matched NTEE2 codes
#' 
#' @export

parse_ntee <- function(ntee.group, ntee.code, ntee.orgtype){

  ntee_code_ls <- ntee_preproc()
  
  # Validate user inputs
  validate_inp(
    ntee.group = ntee.group,
    ntee.code = ntee.code,
    ntee.orgtype = ntee.orgtype,
    ind_group_codes = ntee_code_ls[[2]],
    level_2_4_codes = ntee_code_ls[[3]],
    org_type_codes = ntee_code_ls[[4]]
  )

  # Generate regex queries if inputs are valid
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
