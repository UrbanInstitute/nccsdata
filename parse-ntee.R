
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

# Extract digits23 and digits 45
digits23 <- substring(ntee_disagg_df$old.code, 2, 3)
digits45 <- substring(ntee_disagg_df$old.code, 4, 5)

#' Create function to parse user-inputs and return NTEE Codes
parse_ntee <- function(ntee.group, ntee.code, ntee.orgtype){
  # Formulate regex query based on user input
  
  universal_query <- ""
  
}

