#' Script to return Block or Tract IDs from Census tables that match
#'
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
#' Optional Parameters for the Census Tract
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
#'
#' @import data.table
#' @import dplyr
#' @import purrr
#' @import usdata
#' @import stringr
#'
#' @export

parse_geo <- function(census.level, ...){

  # Check if data is already preloaded

  if (objs_exist("block_dat", "tract_dat")){

    message("Objects in memory")

  } else if (all(file.exists("data/tract_dat.rda", "data/block_dat.rda"))){

    message("Objects not in memory, checking disk")

    block_dat <- load("data/block_dat.rda")
    tract_dat <- load("data/tract_dat.rda")

    message("Block and Tract datasets loaded from disk")

  } else {

    message("Datasets not in disk. Pulling data from S3")

    geo_data_get()
    load("data/block_dat.rda")
    load("data/tract_dat.rda")

    message("Block and Tract datasets loaded")
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
    fips <- dat_filter(
      dat = tract_dat,
      args = args,
      ex_args = ex_args,
      id_col = "tract.census.geoid",
      census.level = census.level
    )
  } else if (geo.level == "BLOCK"){
    fips <- dat_filter(
      dat = block_dat,
      args = args,
      ex_args = ex_args,
      id_col = "block.census.geoid",
      census.level = census.level
    )
  } else {
    stop("Invalid geo.level, select either 'BLOCK' or 'TRACT'")
  }

  return(fips)
}
