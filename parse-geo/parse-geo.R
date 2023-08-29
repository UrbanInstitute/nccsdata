#' Script to return Block or Tract IDs from Census tables that match
#' user inputs
#' 
#' 
#' Load packages
library("data.table")
library("dplyr")
library("purrr")
library("usdata")
library("stringr")
library("aws.s3")

#' Preprocessing
geo_preproc <- function(
    block_s3_url = "s3://nccsdata/geo/xwalk_geoid/block_crosswalk.csv",
    tract_s3_url = "s3://nccsdata/geo/xwalk_geoid/tract_crosswalk.csv"
){
  # Load Data from S3
  block_dt <- 
    save_object(block_s3_url) %>%
    data.table::fread()
  tract_dt <- 
    save_object(tract_s3_url) %>%
    data.table::fread()
  
  #' Rename columns and create state abbreviations in tract
  tract_dt <- tract_dt %>% 
    dplyr::rename_all(
      list(
        ~ paste0("geo.", .)
      )
    ) %>% 
    dplyr::mutate(geo.state = usdata::state2abbr(geo.state_name))
  # Rename columns in block
  block_dt <- block_dt %>% 
    dplyr::rename_all(
      ~ stringr::str_replace_all(., "_geoid", "")
    ) %>% 
    dplyr::rename_all(
      ~ paste0("geo.", .)
    )
  
  return("Data Loaded")
  
}

#' Parse-Geo function
#' Takes as input a series of geographic arguments and returns a list
#' of Tract or Block IDs
parse_geo <- function(geo.level, ...){
  
  # Extract arguments
  args <- enquos(...)
  ex_args <- unname(
    purrr::imap(
      args,
      function(expr, name) quo(!!sym(name)==!!expr)
    )
  )
  
  # Evaluate arguments
  
  # Filter data
  ifelse(
    geo.level == "TRACT",
    tract_dt %>% 
      filter(!!! ex_args) %>% 
      select("tract"),
    ifelse(
      geo.level == "BLOCK",
      block_dt %>% 
        filter(!!! ex_args) %>% 
        select("block_geoid"),
      stop("Invalid geo.level, select either 'BLOCK' or 'TRACT'")
    )
  )
}




