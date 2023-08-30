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