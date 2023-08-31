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

#' Function that returns FIPS codes that match dynamic User arguments
#' 
#' @description Filters either the Block or Tract data.tables to return
#' a list of FIPS codes that match conditions specified by the User
#' 
#' Universal Parameters
#' 
#' @param census.level string. data.table to parse; "BLOCK" | "TRACT"
#' 
#' Parameters for the Block Tract
#' 
#' @param block.census.geoid string or vector. Vector of Block IDs (FIPS)
#' @param tract.census.geoid string or vector. Vector of Tract IDs (FIPS)
#' @param zcta.census.geoid string or vector. Vector of county IDs
#' @param place.census.geoid string or vector. Vector of census place IDs
#' @param county.census.geoid string or vector. Vector of County IDs
#' @param vtd.census.geoid string or vector. Vector of Voting District IDs
#' @param urbanrural.census.geoid string or vector. Vector of ZCTA IDs
#' @param urbanrural.nces.geoid string or vector. Vector of NCES Locale IDs
#' 
#' Parameters for the Census Tract
#' 
#' @param tract.census.geoid string or vector. Vector of Tract IDs (FIPS) 
#' @param county.census.geoid string or vector. Vector of County IDs
#' @param puma.census.geoid string or vector. Vector of PUMA IDs
#' @param state.census.geoid string or vector. Vector of state IDs
#' @param state.census.name string or vector. Vector of state names
#' @param metro.census.cbsa.geoid string or vector. Vector of cbsa IDs
#' @param metro.census.cbsa.name string or vector. Vector of census area names
#' @param metro.census.csa.geoid string or vector. Vector of csa IDs
#' @param metro.census.csa.name string or vector. Vector of csa names
#' @param region.woodard.nation string or vector. Vector of region names
#' @param region.woodard.culture string or vector. Vector of culture labels
#' @param region.census.main string or vector. Vector of region names
#' @param region.census.division string or vector. Vector of subregion names
#' @param state.census.abbr string or vector. Vector of state abbreviations
#' 
#' @usage parse_geo(census.level = "TRACT",
#'                  state.census.abbr = c("NY", "MD"))
#' 
#' @return a list of FIPS codes for either Tract IDs or Block IDs.

parse_geo <- function(census.level, ...){
  
  # Check if data is already preloaded
  if (objs_exist("block_dt", "tract_dt")){
    print("Block and Tract datasets present")
  } else {
    geo_data_get()
  }
  
  # Extract arguments
  args <- enquos(...)
  ex_args <- unname(purrr::imap(
      args,
      function(expr, name) quo( !! sym(name) == !! expr)
    )
  )
  
  # Evaluate arguments
  if (census.level == "TRACT"){
    fips <- validate_arg(
      dat = tract_dt,
      args = args,
      ex_args = ex_args,
      id_col = "tract.census.geoid",
      geo.level = geo.level
    )
    } else if (geo.level == "BLOCK"){
    fips <- validate_arg(
      dat = block_dt,
      args = args,
      ex_args = ex_args,
      id_col = "block.census.geoid",
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
