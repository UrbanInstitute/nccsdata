## Script to return Block or Tract IDs from Census tables that match

## Load required packages
library("data.table")
library("dplyr")
library("purrr")
library("usdata")
library("stringr")
library("aws.s3")
library("docstring")
library("roxygen2")

geo_preproc <- function(
    block_s3_url = "s3://nccsdata/geo/xwalk_geoid/block_crosswalk.csv",
    tract_s3_url = "s3://nccsdata/geo/xwalk_geoid/tract_crosswalk.csv"
){
  #' Preprocess Census Block and Tract Crosswalks
  #' 
  #' @description This function downloads the crosswalk datasets from an S3
  #' bucket, reads them as data.tables, reformats the column names, and creates
  #' a new column with state abbreviations for the Tract dataset.
  #' 
  #' @usage geo_preproc()
  #' 
  #' @return A string message indicating that the processed data.tables
  #' are available in memory
  #' 
  #' @note Can also be used to recreate the data.tables if files get corrupted.
  
  message("Loading Datasets")
  
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
    fips <- validate_arg(
      dat = tract_dt,
      args = args,
      ex_args = ex_args,
      id_col = "geo.tract",
      geo.level = geo.level
    )
    } else if (geo.level == "BLOCK"){
    fips <- validate_arg(
      dat = block_dt,
      args = args,
      ex_args = ex_args,
      id_col = "geo.block",
      geo.level = geo.level
    )    
  } else {
    stop("Invalid geo.level, select either 'BLOCK' or 'TRACT'")
  }

  return(list(fips))
}



#' Function to validate user inputs
validate_arg <- function(dat, args = args, ex_args = ex_args, id_col,
                         geo.level){
  if (all(names(args) %in% colnames(dat))){
    parsed_ids <- dat %>% 
      suppressWarnings(filter(!!! ex_args)) %>% 
      select(id_col)
    return(parsed_ids)
  } else {
    absent_colnames <- setdiff(names(args), colnames(dat))
    stop(paste("The following columns are not present in the",
               geo.level,
               "dataset:",
               setdiff(names(args), colnames(dat))))
  }  
}
