#' Preprocess Census Block and Tract Crosswalks
#' 
#' @description This function downloads the crosswalk datasets from an S3
#' bucket, reads them as data.tables, reformats the column names, and creates
#' a new column with state abbreviations for the Tract dataset.
#' 
#' @param block_s3_url string. Path to S3 bucket with block crosswalk.
#' @param tract_s3_url string. Path to S3 bucket with tract crosswalk.
#' 
#' @usage geo_preproc()
#' 
#' @return A string message indicating that the processed data.tables
#' are available in memory
#' 
#' @note Can also be used to recreate the data.tables if files get corrupted.

geo_preproc <- function(
    block_s3_url = "s3://nccsdata/geo/xwalk_geoid/block_crosswalk.csv",
    tract_s3_url = "s3://nccsdata/geo/xwalk_geoid/tract_crosswalk.csv"
){
  
  message("Loading Datasets")
  
  # Load Data from S3
  
  block_dt <- 
    save_object(block_s3_url) %>%
    data.table::fread()
  
  tract_dt <- 
    save_object(tract_s3_url) %>%
    data.table::fread()
  
  # Rename columns and create state abbreviations in tract data.table
  tract_dt <- tract_dt %>% 
    dplyr::rename_all(
      list(
        ~ paste0("geo.", .)
      )
    ) %>% 
    dplyr::mutate(geo.state = usdata::state2abbr(geo.state_name))
  
  # Rename columns in block data.table
  block_dt <- block_dt %>% 
    dplyr::rename_all(
      ~ stringr::str_replace_all(., "_geoid", "")
    ) %>% 
    dplyr::rename_all(
      ~ paste0("geo.", .)
    )
  
  return("Data Loaded")
  
}