
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

#' Read csv file to extract new NTEE Codes

ntee_disagg_df <- read.csv("ntee-disaggregated.csv")
ntee_new__codes <- ntee_disagg_df$new.code

#' Create function to parse user-inputs and return NTEE Codes
parse_ntee <- function(ntee.group, ntee.code, ntee.orgtype){
  
}

