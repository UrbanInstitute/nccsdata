#' Function to inspect user inputs and flag errors
#' 
#' @description This function validates user inputs for Industry Group, 
#' Industry, Division, Subdivision and Organization Type, comparing them
#' to the set of codes in the population. It throws informative error
#' messages.
#' 
#' @param ntee.group character. 3-character alphabetical Industry Group code.
#' @param ntee.code character. 3-character alphanumeric containing 
#' Industry, Division and Subdivision
#' @param ntee.orgtype character. 2-character alphabetical Organization Type
#' code.
#' @param ind_group_codes. character vector of all acceptable Industry Group
#' codes.
#' @param level_2_4_codes. character vector of all acceptable Industry,
#' Division and Subdivision codes.
#' @param org_type_codes. character vector of all acceptable Organization
#' Type codes.
#' 
#' @usage validate_inp(ntee.group, ntee.code, ntee.orgtype, ind_group_codes,
#'                     level_2_4_codes, org_type_codes)
#' @return String indicating whether input is valid or invalid. If invalid,
#' points user to a list of acceptable codes. 

validate_inp <- function(ntee.group,
                         ntee.code,
                         ntee.orgtype,
                         ind_group_codes,
                         level_2_4_codes,
                         org_type_codes){
  
  if (any(ntee.group %in% c(ind_group_codes, "all"))){
    message("Collecting Matching Industry Groups")
  } else {
    stop("Invalid Industry Group \n 
          List of available groups can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md")
  }
  
  if (any(ntee.code %in% c(level_2_4_codes, "all")) | 
      any(grepl("[A-Z][0-9xX]*[A-Z0-9xX]*", ntee.code))){
    message("Collecting Matching Industry Division and Subdivisions")
  } else {
    stop("Invalid Industry Division Subdivision Combination \n 
          List of available Combinations can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md")
  }
  
  if (any(ntee.orgtype %in% c(org_type_codes, "all"))) {
    message("Collecting Matching Organization Types")
  } else {
    stop("Invalid Organization Type \n 
          List of available Organization Types can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md")
    
  }
  
}
