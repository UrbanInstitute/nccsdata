#' Script to return Block or Tract IDs from Census tables that match
#'
#' Function that returns FIPS codes that match dynamic User arguments
#'
#' @description Filters either the Block or Tract data.tables to return
#' a list of FIPS codes that match conditions specified by the User
#'
#' @param census.level string. data.table to parse; "BLOCK" | "TRACT"
#' @param ... columns in either tract or block dataset for filtering.
#'
#' @usage parse_geo(census.level, ...)
#'
#' @examples  parse_geo(census.level = "TRACT",
#'                      state.census.abbr = c("NY", "MD"))
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

  # Extract arguments
  args <- enquos(...)
  ex_args <- unname(purrr::imap(
    args,
    function(expr, name) quo( !! sym(name) == !! expr)))

  # Evaluate arguments
  if (census.level == "TRACT"){
    fips <- dat_filter(
      dat = tract_dat,
      args = args,
      ex_args = ex_args,
      id_col = "tract.census.geoid",
      census.level = census.level
    )
  } else if (census.level == "BLOCK"){
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
