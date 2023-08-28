
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
library("rjson")


#' Read json file and extract old codes
ntee_old <- fromJSON(file = "ntee.json")
ntee_old <- names(ntee_old)
