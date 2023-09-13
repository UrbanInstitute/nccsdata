#' @title Function to query bmf data and integrate it with census, cbsa and ntee
#' metadata
#'
#' @description This function uses user inputs to query, filter and merge nccs
#' data and additional census, cbsa and ntee metadata
#'
#' @param ntee.level1 string or vector. Nonprofit Industry Group. Default ==
#' "all" includes all Industry Groups.
#' @param ntee.level2 string or vector. Level 2-4 of NTEE code (Industry,
#' Division and Subdivision). Default == "all" includes all codes.
#' @param geo.state string or vector. Filter query by state abbreviations e.g.
#' "NY", "CA". Default == NULL includes all states.
#' @param geo.metro string or vector. Filter query by cbsa code. Default = NULL
#' includes all metro cbsa codes.
#' @param geo.level string. Census dataset to merge with. Default == "tract"
#' which merges filtered bmf data with census tract data.
#'
#' @return data.table with queried data
#'
#' @usage get_data(ntee.level1,
#'                 ntee.level2,
#'                 geo.state,
#'                 geo.metro,
#'                 geo.level)
#'
#' @export
#'
#' @importFrom data.table setDT
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' @import dtplyr
#' @import dplyr

get_data <- function(ntee.level1 = "all",
                     ntee.level2 = "all",
                     geo.state = NULL,
                     geo.metro = NULL,
                     geo.level = "tract"){

  # load in datasets as data.table
  tinybmf_dat <- data.table::setDT(tinybmf)
  tract_dat <- data.table::setDT(tract_dat)
  block_dat <- data.table::setDT(block_dat)
  ntee_dat <- data.table::setDT(ntee_df)
  cbsa_dat <- data.table::setDT(cbsa_df)

  # Data wrangling

  # bmf data

  tinybmf_dat <- tinybmf_dat %>%
    dplyr::rename("tract.census.geoid" = .data$TRACT.GEOID.10,
                  "block.census.geoid" = .data$BLOCK.GEOID.10,
                  "state.census.abbr" = .data$STATE,
                  "ntee2.code" = .data$NTEE2) %>%
    dplyr::mutate(across(c("tract.census.geoid", "block.census.geoid"),
                         stringr::str_replace,
                         "GEO-",
                         ""))
  # cbsa data

  cbsa_ex_cols <- setdiff(colnames(cbsa_df), colnames(tract_dat))
  cbsa_dat <- cbsa_dat %>%
    dplyr::select(append(.data$metro.census.cbsa.geoid, cbsa_ex_cols)) %>%
    group_by(.data$metro.census.cbsa.geoid)

  # Apply NTEE filters
  if (any(! ntee.level1 == "all" | ! ntee.level2 == "all")){
  ntee2_codes <- parse_ntee(ntee.group = ntee.level1,
                            ntee.code = ntee.level2,
                            ntee.orgtype = "all")
  tinybmf_subset <- tinybmf_dat %>%
    dplyr::filter(.data$ntee2.code %in% ntee2_codes) %>%
    dplyr::left_join(ntee_dat, by = "ntee2.code")
  } else {
    tinybmf_subset <- tinybmf_dat %>%
      dplyr::left_join(ntee_dat, by = "ntee2.code")
  }

  # Apply geographic filters


  # State filter
  if (! is.null(geo.state)) {
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::filter(.data$state.census.abbr %in% geo.state)
  }

  # Census level
  if (geo.level == "tract"){
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(tract_dat, by = "tract.census.geoid")
  } else if (geo.level == "block") {
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(block_dat, by = "block.census.geoid")
  } else if (geo.level == "both") {
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(tract_dat, by = "tract.census.geoid") %>%
      dplyr::left_join(block_dat, by = "block.census.geoid")
  } else {
    stop("Invalid geo.level, select either 'block', 'tract' or 'both'")
  }

  # CBSA filter
  if (! is.null(geo.metro)){
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(tract_dat, by = "tract.census.geoid") %>%
      dplyr::select(.data$metro.census.cbsa.geoid %in% geo.metro) %>%
      dplyr::group_by("metro.census.cbsa.geoid") %>%
      dplyr::left_join(cbsa_dat, by = "metro.census.cbsa.geoid") %>%
      dplyr::ungroup()
  }


  return(tinybmf_subset)

}
