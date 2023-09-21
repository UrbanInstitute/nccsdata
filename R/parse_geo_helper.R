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
      suppressWarnings(dplyr::filter(!!! ex_args)) %>%
      dplyr::select(dplyr::all_of(id_col))

    return(parsed_ids)

  } else {

    absent_colnames <- setdiff(names(args), colnames(dat))

    stop(paste("The following columns are not present in the",
               census.level,
               "dataset:",
               setdiff(names(args), colnames(dat))))
  }
}


#' @title function that maps fips codes to user arguments
#'
#' @description This function takes in the geographic arguments from get_data()
#' and maps them to fips codes found in the core datasets. These codes can
#' then be used to filter the core datasets.
#'
#' @param geo.state character vector. State abbreviations for filtering e.g.
#' "NY", "CA".
#' @param geo.city character vector. City names for filtering e.g. "Chicago",
#' "montgomery". Case insensitive
#' @param geo.county character vector. County names for filtering e.g.
#' "cullman", "dale". Case insensitive.
#'
#' @return character vector. county fips codes for filtering core datasets.
#'
#' @usage fips_map(geo.state, geo.city, geo.county)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull

fips_map <- function(geo.state,
                     geo.city,
                     geo.county){


  cbsa_fips_all <- c()

  if (! is.null(geo.state)){

    cbsa_fips <- cbsa_df %>%
      dplyr::filter(.data$state.census.abbr %in% geo.state) %>%
      dplyr::pull(.data$metro.census.cbsa.geoid)

    cbsa_fips_all<- c(cbsa_fips_all,
                      cbsa_fips)

  }

  if (! is.null(geo.city)){

    city_str_filter <- paste(tolower(geo.city),
                             collapse = "|")
    cbsa_fips <- cbsa_df %>%
      dplyr::mutate("metro.census.cbsa.name" = tolower(.data$metro.census.cbsa.name)) %>%
      dplyr::filter(grepl(city_str_filter, .data$metro.census.cbsa.name)) %>%
      dplyr::pull(.data$metro.census.cbsa.geoid)

    cbsa_fips_all <- c(cbsa_fips_all, cbsa_fips)

  }

  if (! is.null(geo.city)){

    county_str_filter <- paste(tolower(geo.county),
                               collapse = "|")
    cbsa_fips <- cbsa_df %>%
      dplyr::mutate("census.county.name" = tolower(.data$census.county.name)) %>%
      dplyr::filter(grepl(county_str_filter, .data$census.county.name)) %>%
      dplyr::pull(.data$metro.census.cbsa.geoid)

    cbsa_fips_all <- c(cbsa_fips_all, cbsa_fips)

  }

  county_fips <- tract_dat %>%
    dplyr::filter(.data$metro.census.cbsa.geoid %in% cbsa_fips_all) %>%
    dplyr::pull(.data$county.census.geoid)

  county_fips <- unlist(lapply(county_fips,
                               function(x) ifelse(nchar(x) == 4,
                                                  paste0("0", x),
                                                  x)))

  return(county_fips)
}
