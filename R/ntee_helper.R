#' Script containing helper functions for ntee_main.R
#' 
#' This function preprocesses the disaggregated NTEE dataset and returns
#' a vector of NTEE codes formatted according to NTEE2 standards.
#' 
#' @description This function reads in a csv containing disaggregated NTEE
#' codes built from the old formatting. It uses the rules specified in
#' https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md
#' to derive the codes from level 1-5 used in NTEE2 from old NTEE codes
#' 
#' @param path_to_csv character. Path to the .csv file with disaggregated NTEE
#' data
#' 
#' @usage ntee_preproc(path_to_csv)
#' 
#' @return A list containing the population of valid NTEE2 codes
#' 
#' @note The disaggregated csv file can be edited and the function rerun
#' to generate a new list of valid NTEE2 codes if needed.
#' 
#' @export

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
  
  # Append NTEE2 codes to disaggregated csv and save
  ntee_disagg_df$ntee2.code <- ntee_new_codes
  saveRDS(ntee_disagg_df, "../data-raw/ntee_df.RDS")
  
  return(list(ntee_new_codes, 
              ntee2_level1, 
              ntee2_level_2_4, 
              ntee2_level5))
}