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
source("geo_data_get.R")
source("../utils/objs_exist.R")

parse_geo <- function(geo.level, ...){
  
  #' Function that returns FIPS codes that match dynamic User arguments
  #' 
  #' @description Filters either the Block or Tract data.tables to return
  #' a list of FIPS codes that match conditions specified by the User
  #' 
  #' Parameters for the Block Tract
  #' 
  #' @param geo.level character. data.table to parse; "BLOCK" | "TRACT"
  #' @param geo.block string or vector. Vector of Block IDs (FIPS)
  #' @param geo.tract string or vector. Vector of Tract IDs (FIPS)
  #' @param geo.county string or vector. Vector of county IDs
  #' @param geo.place string or vector. Vector of census place IDs
  #' @param geo.ua string or vector. Vector of Urban Area IDs
  #' @param geo.vtd string or vector. Vector of Voting District IDs
  #' @param geo.zcta string or vector. Vector of ZCTA IDs
  #' @param geo.locale string or vector. Vector of NCES Locale IDs
  #' 
  #' Parameters for the Census Tract
  
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
