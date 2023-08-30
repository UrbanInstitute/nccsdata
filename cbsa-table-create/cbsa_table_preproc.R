## Script to preprocess NBER cbsa crosswalk

cbsa_df <- read.csv("cbsa2fipsxw.csv")

library("tidyr")
library("dplyr")
library("jsonlite")

# Nest dataframe by cbsacode and write to json

cbsa_nest_df <- cbsa_df %>% 
  dplyr::group_nest(cbsacode)

cbsa_nest_json <- cbsa_nest_df %>% 
  jsonlite::toJSON() %>% 
  write("cbsa.json")