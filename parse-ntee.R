
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

#' Read csv file to extract different versions of NTEE Codes

ntee_disagg_df <- read.csv("ntee-disaggregated.csv")
ntee2_level1 <- ntee_disagg_df$broad.category
ntee2_level2 <- ntee_disagg_df$major.group

#' Extract digits23 and digits 45
digits23 <- substring(ntee_disagg_df$old.code, 2, 3)
digits45 <- substring(ntee_disagg_df$old.code, 4, 5)
digits45 <- replace(digits45, digits45 == "", "00")


#' Use digits23 and digits45 to get level 3 of new NTEE code
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



#' Create function to parse user-inputs and return NTEE Codes
parse_ntee <- function(ntee.group, ntee.code, ntee.orgtype){
  # Formulate regex query based on user input
  
  universal_query <- ""
  
}

