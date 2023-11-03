#' Function to filter data table
#'
#' @description dat_filter() takes in arguments specified by the user (name of
#' dataset, column names and subsets), checks if those filters apply to the
#' specified dataset and returns FIPs codes that meet the subsetting conditions.
#'
#' A user specifies whether they are pulling data from the Block or Tract
#' datasets and specifies the subset of the data they want e.g. (geo.state.abbr
#' = c("NY")). The function returns either the Tract or Block IDs that fit
#' the subset specified
#'
#' @param dat data table. Data table of either census or block data.
#' @param args string expression. User arguments specified in parse_geo()
#' @param ex_args string expression. Processed user arguments specified in
#' parse_geo()
#' @param id_col string. Name of column with Tract or Block IDs
#' @param census.level name of data.table (Block/Census) for error messages.
#'
#' @usage dat_filter(dat,
#'                   args,
#'                   ex_args,
#'                   id_col,
#'                   census.level)
#' @returns A list of fips codes based on filter criteria.
#'
#' @import dplyr

dat_filter <- function(dat,
                       args = args,
                       ex_args = ex_args,
                       id_col,
                       census.level){

  if (all(names(args) %in% colnames(dat))){

    parsed_ids <- dat %>%
      dplyr::filter(!!! ex_args) %>%
      dplyr::pull(id_col)

    return(parsed_ids)

  } else {

    absent_colnames <- setdiff(names(args), colnames(dat))

    stop(paste("The following columns are not present in the",
               census.level,
               "dataset:",
               setdiff(names(args), colnames(dat))))
  }
}

#' @title Simplified Geo Parser for Census Crosswalks
#'
#' @description This function takes in simplified user inputs, filters
#' either the tract or block datasets based on these inputs and returns the
#' relevant FIPS codes
#'
#' @param census.level character scalar. Name of crosswalk, either 'tract' or
#' 'block'.
#' @param geo.region character vector. Name of geographic region. Acceptable
#' values are 'South', 'West', 'Northeast' and 'Midwest'. Default == NULL.
#' @param geo.state character vector. 2 letter state abbreviations. Values such
#' as 'NY', 'CA', 'WY' are accepted. Default == NULL.
#' @param geo.county character vector. Name of county. Case insensitive.
#' Default == NULL.
#' @param geo.city character vector. Name of city. State should be entered
#' in geo.state. Case insensitive. Default == NULL.
#'
#' @examples
#' \dontrun{
#' parse_geo(census.level = "tract",
#'           geo.region = "West",
#'           geo.state = "WA",
#'           geo.county = "king")
#' }
#'
#' @returns a character vector of either census tract or block FIPS.
#'
#' @details All arguments are nested. Hence only the FIPs that are present in
#' all filters are returned.
#'
#' @importFrom stringr str_to_title
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom dplyr %>%

parse_geo <- function(census.level,
                      geo.region = NULL,
                      geo.state = NULL,
                      geo.county = NULL,
                      geo.city = NULL){


  stopifnot("Invalid census level. Select either 'block' or 'tract'" =
              (census.level == "block" | census.level == "tract"))

  df_filtered <- tract_dat

  if (! is.null(geo.region)){

    geo.region = stringr::str_to_title(geo.region)
    stopifnot("Invalid region. Select 'South', 'West', 'Northeast' or 'Midwest" =
                all(geo.region %in% unique(tract_dat$region.census.main)))

    df_filtered <- df_filtered %>%
      dplyr::filter(.data$region.census.main %in% geo.region)

  }

  if (! is.null(geo.state)){

    stopifnot("Invalid state. Use state abbreviations. E.g. 'NY' for New York" =
                all(geo.state %in% unique(tract_dat$state.census.abbr)))

    df_filtered <- df_filtered %>%
      dplyr::filter(.data$state.census.abbr %in% geo.state)

  }

  if (! is.null(geo.county)){

    geo.county <- stringr::str_to_title(geo.county)

    cbsa_id <- cbsa_df %>%
      dplyr::filter(grepl(paste(geo.county, collapse = "|"),
                          .data$census.county.name)) %>%
      dplyr::pull("metro.census.cbsa.geoid")

    df_filtered <- df_filtered %>%
      dplyr::filter(.data$metro.census.cbsa.geoid %in% cbsa_id)

  }

  if (! is.null(geo.city)){

    geo.city <- stringr::str_to_title(geo.city)

    cbsa_id <- cbsa_df %>%
      dplyr::filter(grepl(paste(geo.city, collapse = "|"),
                          .data$metro.census.cbsa.name)) %>%
      dplyr::pull("metro.census.cbsa.geoid")

    df_filtered <- df_filtered %>%
      dplyr::filter(.data$metro.census.cbsa.geoid %in% cbsa_id)

  }

  id <- df_filtered %>%
    dplyr::pull("tract.census.geoid")

  if (census.level == "block"){

    id <- block_dat %>%
      dplyr::filter(.data$tract.census.geoid %in% id) %>%
      dplyr::pull("block.census.geoid")

  }

  return(id)

}

#' @title Function that returns FIPS codes that match dynamic User arguments
#'
#' @description Filters either the Block or Tract data.tables to return
#' a list of FIPS codes that match conditions specified by the User
#'
#' @param census.level string. data.table to parse; "BLOCK" | "TRACT"
#' @param ... columns in either tract or block dataset for filtering.
#'
#' @usage parse_fips(census.level, ...)
#'
#' @examples
#' \dontrun{
#' parse_fips(census.level = "TRACT",
#'            state.census.abbr = c("NY", "MD"))
#' }
#'
#' @return a list of FIPS codes for either Tract IDs or Block IDs.
#'
#' @importFrom purrr imap

parse_fips <- function(census.level, ...){

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
