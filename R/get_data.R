#' @title Function to query bmf data and integrate it with census, cbsa and ntee
#' metadata
#'
#' @description This function uses user inputs to query, filter and merge nccs
#' data and additional census, cbsa and ntee metadata
#'
#' @param dsname character scalar. Name of data series to query from S3.
#' Valid inputs are "core" and "bmf", not both.
#' @param time character vector. Dates of core/bmf files to query. Valid
#' inputs range from 1989-2022. Default value is "current" for 2022.
#' @param scope.orgtype character scalar. Organization type to query from
#' core/bmf s3 bucket. Valid inputs are 'CHARITIES' for charities (501C3-PC),
#' 'PRIVFOUND' for private foundations (501C3-PF) and 'NONPROFIT' for all
#' nonprofits (501CE)
#' @param scope.formtype character scalar. Form type to query from core/bmf s3
#' bucket. Valid inputs are 'PC'(nonprofits that file the full version),
#' 'EZ'(nonprofits that file 990EZs only), '
#' PZ'(nonprofits that file both PC and EZ), or 'PF'(private foundations).
#' @param geo.state character vector. Filter query by state abbreviations e.g.
#' "NY", "CA". Default == NULL includes all states.
#' @param ntee. character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
#' @param ntee.group character vector. Specific Industry Group codes submitted
#' by user
#' @param ntee.code character vector. Specific level 2-4 codes (Industry,
#' Division, Subdivision) submitted by user.
#' @param ntee.orgtype character vector. Specific level 5 codes (Organization
#' Type) submitted by user.
#'
#' @return data.table with queried data
#'
#' @usage get_data(dsname,
#'                 time,
#'                 scope.orgtype,
#'                 scope.formtype,
#'                 geo.state,
#'                 ntee,
#'                 ntee.group,
#'                 ntee.code,
#'                 ntee.orgtype)
#'
#' @export
#'
#' @importFrom data.table setDT
#' @importFrom stringr str_replace
#' @importFrom rlang .data
#' @importFrom dplyr filter

get_data <- function(dsname = NULL,
                     time = "current",
                     scope.orgtype = "NONPROFIT",
                     scope.formtype = "PZ",
                     geo.state = NULL,
                     ntee = NULL,
                     ntee.group = NULL,
                     ntee.code = NULL,
                     ntee.orgtype = NULL){

  # Validate inputs
  valid_msg <- validate_get_data(dsname = dsname,
                                 time = time,
                                 scope.orgtype = scope.orgtype,
                                 scope.formtype = scope.formtype)
  message(valid_msg)

  # NTEE parsing
  ntee2_matches <- query_ntee(ntee.user = ntee,
                              ntee.group = ntee.group,
                              ntee.code = ntee.code,
                              ntee.orgtype = ntee.orgtype)

  # Geo parsing

  # Put into seperate function
  nteecc_df <- ntee_df %>%
    dplyr::filter(.data$ntee2.code %in% ntee2_matches)
  nteecc_matches <- nteecc_df$old.code

  if (dsname == "core"){
    filenames <- core_file_constructor(time = time,
                                       scope.orgtype = scope.orgtype,
                                       scope.formtype = scope.formtype)
  }

  # Check if files exist
  keys <- s3_validate(dsname = dsname,
                      filenames = filenames)

  # Query keys
  select_results <- s3_query(bucket = "nccsdata",
                             keys = keys,
                             geo.state = geo.state,
                             ntee.cc = nteecc_matches)

  # Merge with ntee dataset

  # Merge with geo dataset

  return(select_results)

}


