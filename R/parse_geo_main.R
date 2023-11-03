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

#' @title function that maps county FIPS codes to user arguments
#'
#' @description This function takes in the geographic arguments
#' and maps them to county FIPS codes found in legacy datasets. These codes can
#' then be used to filter these legacy datasets.
#'
#' @param geo.county character vector. County names for filtering e.g.
#' "cullman", "dale". Case insensitive.
#' @param geo.region character vector. Regions for filtering e.g. "South",
#' "Midwest" based on census region classifications.
#' @param geo.cbsafips numeric vector. Census CBSA FIPS codes.
#' @param geo.csafips numeric vector. Census CSA FIPS codes.
#'
#' @return character vector. county fips codes for filtering core datasets.
#'
#' @usage map_countyfips(geo.county, geo.region, geo.cbsafips, geo.csafips)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#'
#' @export

map_countyfips <- function(geo.county = NULL,
                           geo.region = NULL,
                           geo.cbsafips = NULL,
                           geo.csafips = NULL){

  county_fips <- tract_dat

  if (! is.null(geo.county)){

    county_str_filter <- paste(tolower(geo.county),
                               collapse = "|")
    cbsa_fips <- cbsa_df %>%
      dplyr::mutate("census.county.name" = tolower(.data$census.county.name)) %>%
      dplyr::filter(grepl(county_str_filter, .data$census.county.name)) %>%
      dplyr::pull("metro.census.cbsa.geoid")

    county_fips <- county_fips %>%
      dplyr::filter(.data$metro.census.cbsa.geoid %in% cbsa_fips)


  }

  if (! is.null(geo.region)){

    county_fips <- county_fips %>%
      dplyr::filter(.data$region.census.main %in% geo.region)

  }

  if (! is.null(geo.cbsafips)){

    county_fips <- county_fips %>%
      dplyr::filter(.data$metro.census.cbsa.geoid %in% geo.cbsafips)

  }

  if (! is.null(geo.csafips)){

    county_fips <- county_fips %>%
      dplyr::filter(.data$metro.census.csa.geoid %in% geo.csafips)

  }

  county_fips <- county_fips %>%
    dplyr::pull("county.census.geoid")

  return(unique(county_fips))
}
