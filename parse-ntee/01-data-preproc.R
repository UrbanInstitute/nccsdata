#' This function preprocesses the disaggregated NTEE dataset and returns
#' a vector of NTEE codes formatted according to NTEE2 standards.
#' 
ntee_preproc <- function(path_to_csv = "ntee-disaggregated.csv"){
  
  # Read csv file to extract different versions of NTEE Codes
  ntee_disagg_df <- read.csv("ntee-disaggregated.csv")
  
  # Extract level 1 and level 2 parts of NTEE2 Code
  ntee2_level1 <- ntee_disagg_df$broad.category
  ntee2_level2 <- ntee_disagg_df$major.group
  
  # Extract digits23 and digits 45
  digits23 <- substring(ntee_disagg_df$old.code, 2, 3)
  digits45 <- substring(ntee_disagg_df$old.code, 4, 5)
  digits45 <- replace(digits45, digits45 == "", "00")
  
  # Use digits23 and digits45 to get level 3 and 4 of new NTEE code
  ntee2_level_3_4 <- mapply(get_ntee_level_3_4, digits23, digits45)
  
  # Combine levels 2-4
  ntee2_level_2_4 <- paste(ntee2_level2, ntee2_level_3_4, sep = "")
  
  # Extract level 5 code from disaggregated csv
  ntee2_level5 <- ntee_disagg_df$type.org
  
  # Create population of NTEE2 codes
  ntee_new_codes <- paste(
    ntee2_level1,
    "-",
    ntee2_level2,
    ntee2_level_3_4,
    "-",
    ntee2_level5,
    sep = ""
  )
  
  return(list(ntee_new_codes, 
              ntee2_level1, 
              ntee2_level_2_4, 
              ntee2_level5))
}