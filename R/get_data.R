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

get_data <- function(dsname = NULL,
                     time = "current",
                     scope.orgtype = "NONPROFIT",
                     scope.formtype = "PC"){

  # Validate inputs
  valid_msg <- validate_get_data(dsname = dsname,
                                 time = time,
                                 scope.orgtype = scope.orgtype,
                                 scope.formtype = scope.formtype)
  message(valid_msg)

  return("test")

}

#' @title Function to pull core data from S3 bucket.

