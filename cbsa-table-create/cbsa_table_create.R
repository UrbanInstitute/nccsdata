## Script to preprocess NBER cbsa crosswalk
library("tidyr")
library("dplyr")
library("jsonlite")
library("usdata")
source("../utils/jsonify.R")

#' Function to create cbsa arguments table and save to json
#' 
#' @description This function reads the path to the raw cbsa .csv
#' renames the columns, saves the processed data as an .RDS, nests
#' the dataframe by cbsa ID and saves the nested data as a .json file.
#' 
#' @param path_to_raw_cbsa string. Path to raw cbsa data.
#' 
#' @usage cbsa_table_create(path_to_raw_cbsa)
#' 
#' @return Message indicating that data has been processed and saved

cbsa_table_create <- function(path_to_raw_cbsa = "cbsa2fipsxw.csv"){
  
  # Read csv
  cbsa_df <- read.csv(path_to_raw_cbsa)
  
  # Preprocess Columns
  cbsa_df <- cbsa_df %>% 
    rename(metro.census.cbsa.geoid = cbsacode,
           metro.div.geoid = metropolitandivisioncode,
           metro.census.csa.geoid = csacode,
           metro.census.cbsa.name = cbsatitle,
           metro.micro.name = metropolitanmicropolitanstatis,
           metro.div.name = metropolitandivisiontitle,
           metro.census.csa.name = csatitle,
           census.county.name = countycountyequivalent,
           state.census.name = statename,
           state.censu.geoid = fipsstatecode,
           census.county.geoid = fipscountycode,
           census.centrout.name = centraloutlyingcounty) %>% 
    mutate(state.census.abbr = usdata::state2abbr(state.census.name))
  
  # Save as RDS
  
  saveRDS(cbsa_df, "cbsa_df.RDS")
  message("Dataframe preprocessed and saved as .RDS")
  
  # Nest dataframe by cbsacode and write to json
  
  cbsa_nest_df <- cbsa_df %>% 
    dplyr::group_nest(metro.census.cbsa.geoid)
  
  cbsa_nest_json <- cbsa_nest_df %>% 
    jsonlite::toJSON() %>% 
    jsonify_f()
  
  write(cbsa_nest_json, "cbsa.json")
  
  return(message("Data processed and written to cbsa.json"))
  
}







