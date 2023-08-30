## Script to preprocess NBER cbsa crosswalk

cbsa_df <- read.csv("cbsa2fipsxw.csv")

library("tidyr")
library("dplyr")
library("jsonlite")
library("usdata")

#' Function to create cbsa arguments table and save to json
#' 

cbsa_table_create <- function(path_to_raw_cbsa){
  
  # Read csv
  cbsa_df <- read.csv(path_to_raw_cbsa)
  
  # Preprocess Columns
  cbsa_df <- cbsa_df %>% 
    rename(cbsa_code = cbsacode,
           metropolitan_and_division_code = metropolitandivisioncode,
           csa_code = csacode,
           cbsa_title = cbsatitle,
           metropolitan_micropolitan_statis = metropolitanmicropolitanstatis,
           metropolitan_division_title = metropolitandivisiontitle,
           csa_title = csatitle,
           county_county_equivalen = countycountyequivalent,
           state_name = statename,
           fips_state_code = fipsstatecode,
           fips_county_code = fipscountycode,
           central_outlying_county = centraloutlyingcounty) %>% 
    mutate(state_abbr = usdata::state2abbr(state_name))
  
  # Nest dataframe by cbsacode and write to json
  
  cbsa_nest_df <- cbsa_df %>% 
    dplyr::group_nest(cbsa_code)
  
  cbsa_nest_json <- cbsa_nest_df %>% 
    jsonlite::toJSON() %>% 
    jsonify_f()
  
  write(cbsa_nest_json, "cbsa.json")
  
  return(message("Data processed and written to cbsa.json"))
  
}




jsonify_f <- function(f)
{
  f <- as.factor(f)
  f_level <- levels(f)
  f_level <- gsub( "'", "", f_level ) # remove quotes
  f_level <- gsub( '"', '', f_level ) # remove apostrophes
  label <- f_level
  d <- data.frame(f_level,label)
  jd <- jsonlite::toJSON( d )
  jd <- gsub( "\\{", "  \\{  ", jd )
  jd <- gsub( ":", " :  ", jd )
  jd <- gsub( '",', '"  ,  ', jd )
  jd <- gsub( '","', '",  "', jd )
  jd <- gsub( "\\},", "  \\}, \n", jd )
  jd <- gsub( "\\[", "\\[ \n", jd )
  jd <- gsub( "\\]", "\n\\]", jd )
  jd <- gsub( "\\}\n", "  \\}\n", jd )
  return(jd)
}


