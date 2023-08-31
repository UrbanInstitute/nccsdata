## Script to preprocess NBER cbsa crosswalk
library("tidyr")
library("dplyr")
library("jsonlite")
library("usdata")
source("../utils/jsonify.R")

#' Function to create cbsa arguments table and save to json
#' 

cbsa_table_create <- function(path_to_raw_cbsa = "cbsa2fipsxw.csv"){
  
  # Read csv
  cbsa_df <- read.csv(path_to_raw_cbsa)
  
  # Preprocess Columns
  cbsa_df <- cbsa_df %>% 
    rename(id = cbsacode,
           metropolitan_and_division_code = metropolitandivisioncode,
           csa_code = csacode,
           cbsa_title = cbsatitle,
           metropolitan_micropolitan_statis = metropolitanmicropolitanstatis,
           metropolitan_division_title = metropolitandivisiontitle,
           csa_title = csatitle,
           county_county_equivalent = countycountyequivalent,
           state_name = statename,
           fips_state_code = fipsstatecode,
           fips_county_code = fipscountycode,
           central_outlying_county = centraloutlyingcounty) %>% 
    mutate(state_abbr = usdata::state2abbr(state_name))
  
  # Nest dataframe by cbsacode and write to json
  
  cbsa_nest_df <- cbsa_df %>% 
    dplyr::group_nest(id)
  
  cbsa_nest_json <- cbsa_nest_df %>% 
    jsonlite::toJSON() %>% 
    jsonify_f()
  
  write(cbsa_nest_json, "cbsa.json")
  
  return(message("Data processed and written to cbsa.json"))
  
}







