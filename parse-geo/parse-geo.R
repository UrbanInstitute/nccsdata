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
  
  print("Loading Datasets")
  
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

#' Function to check if objects exist in memory
exists.m <- function(...) {
  ls <- list(...)
  all(sapply(ls, exists))
}

#' Parse-Geo function
#' Takes as input a series of geographic arguments and returns a list
#' of Tract or Block IDs
parse_geo <- function(geo.level, ...){
  
  # Check if data is already preloaded
  if (exists.m("block_dt", "tract_dt")){
    print("Block and Tract datasets present")
  } else {
    geo_preproc()
  }
  
  # Extract arguments
  args <- enquos(...)
  ex_args <- unname(
    purrr::imap(
      args,
      function(expr, name) quo(!!sym(name)==!!expr)
    )
  )
  
  # Evaluate arguments
  if (geo.level == "TRACT"){
    if (all(names(args) %in% colnames(tract_dt))){
      parsed_ids <- tract_dt %>% 
        filter(!!! ex_args) %>% 
        select("geo.tract")
    } else {
      absent_colnames <- setdiff(names(args), colnames(tract_dt))
      stop(paste("The following columns are not present in the dataset:",
                 setdiff(names(args), colnames(tract_dt))))
    }
    } else if (geo.level == "BLOCK"){
    if (all(names(args) %in% colnames(block_dt))){
      parsed_ids <- block_dt %>% 
        filter(!!! ex_args) %>% 
        select("geo.block")
    } else {
      absent_colnames <- setdiff(names(args), colnames(block_dt))
      stop(paste("The following columns are not present in the dataset:",
                 setdiff(names(args), colnames(block_dt))))
    }     
  } else {
    stop("Invalid geo.level, select either 'BLOCK' or 'TRACT'")
  }

  return(list(parsed_ids))
}



# dummy code to test function evaluations
test_eval <- function(...){
  # Extract arguments
  args <- enquos(...)
  ex_args <- unname(
    purrr::imap(
      args,
      function(expr, name) quo(!!sym(name)==!!expr)
    )
  )
  return(args)
}
