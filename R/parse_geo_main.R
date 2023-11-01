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
#' parse_geo(census.level = "tract",
#'           geo.region = "West",
#'           geo.state = "WA",
#'           geo.county = "king")
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
#'
#' @export

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
#' @examples  parse_fips(census.level = "TRACT",
#'                      state.census.abbr = c("NY", "MD"))
#'
#' @return a list of FIPS codes for either Tract IDs or Block IDs.
#'
#' @importFrom purrr imap
#'
#' @export

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


#' @title Preview CBSA/CSA FIPS Codes
#'
#' @description geo_preview() previews cbsa metadata for use in filtering of
#' core data sets. It also saves the outputs with an invisible return.
#'
#' @param geo Character vector. Vector of columns to preview/return from CBSA
#' data
#' @param within character vector. Vector of state abbreviations to filter data
#' with.
#' @param type character scalar. Scalar variable indicating type of data to return.
#' Acceptable values are "metro" and "micro".
#'
#' @return printout of data or data.frame of filtered cbsa data.frame
#'
#' @examples
#'
#' geo_preview( geo="cbsa", within="LA" )
#' geo_preview( geo=c("county","cbsa"), within="GA", type="metro" )
#' geo_preview( geo=c("cbsa","cbsafips"), within="FL", type="metro" )
#' xx <- geo_preview( geo=c("cbsa","cbsafips"), within="FL", type="metro" )
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom knitr kable
#'
#' @export

geo_preview <- function(geo, within = NULL, type = NULL){

  type_dic = list("metro" = "Metropolitan Statistical Area",
                  "micro" = "Micropolitan Statistical Area")

  cbsa_dic = list("cbsafips" = "metro.census.cbsa.geoid",
                  "csafips" = "metro.census.csa.geoid",
                  "cbsa" = "metro.census.cbsa.name",
                  "csa" = "metro.census.csa.name",
                  "county" = "county")

  stopifnot("Invalid argument for within. Enter a valid state abbreviation" =
            all(within %in% c(cbsa_df$state.census.abbr, NULL)),
            "Invalid argument for type. Select 'metro' or 'micro' " =
            type %in% c(names(type_dic), NULL),
            "Invalid geo column. Available columns are: 'cbsafips', 'csafips',
            'cbsa', 'csa', 'county'" =
            all(geo %in% names(cbsa_dic)))

  df <- cbsa_df %>%
    dplyr::mutate("county" = paste(.data$census.county.name,
                                   .data$state.census.abbr,
                                   .data$census.centrout.name,
                                   sep = ", "))

  if( ! is.null(within) ){
    df <- dplyr::filter(df,
                        .data$state.census.abbr %in% within)
    }

  if( ! is.null(type) ){
    df <- dplyr::filter(df,
                        .data$metro.micro.name == type_dic[[type]])
  }

  df <- df %>%
    dplyr::select(unlist(cbsa_dic[geo])) %>%
    dplyr::distinct() %>%
    dplyr::arrange()

  print( df %>% knitr::kable( align="r" ) )

  return(invisible(df))
}
