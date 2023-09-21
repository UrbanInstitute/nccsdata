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
#' @param geo.city character vector. City names for filtering e.g. "Chicago",
#' "montgomery". Case insensitive
#' @param geo.county character vector. County names for filtering e.g.
#' "cullman", "dale". Case insensitive.
#' @param ntee character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
#' @param ntee.group character vector. Specific Industry Group codes submitted
#' by user
#' @param ntee.code character vector. Specific level 2-4 codes (Industry,
#' Division, Subdivision) submitted by user.
#' @param ntee.orgtype character vector. Specific level 5 codes (Organization
#' Type) submitted by user.
#' @param aws boolean. Whether to use aws.s3::s3_select() in executing queries.
#' Default == FALSE, select TRUE to use s3_select. Must have aws account to use.
#'
#' @return data.table with queried data
#'
#' @usage get_data(dsname,
#'                 time,
#'                 scope.orgtype,
#'                 scope.formtype,
#'                 geo.state,
#'                 geo.city,
#'                 geo.county,
#'                 ntee,
#'                 ntee.group,
#'                 ntee.code,
#'                 ntee.orgtype)
#'
#' @export
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom readr get_csv

get_data <- function(dsname = NULL,
                     time = "current",
                     scope.orgtype = "NONPROFIT",
                     scope.formtype = "PZ",
                     geo.state = NULL,
                     geo.city = NULL,
                     geo.county = NULL,
                     ntee = NULL,
                     ntee.group = NULL,
                     ntee.code = NULL,
                     ntee.orgtype = NULL,
                     aws = FALSE){

  # Validate inputs
  valid_msg <- validate_get_data(dsname = dsname,
                                 time = time,
                                 scope.orgtype = scope.orgtype,
                                 scope.formtype = scope.formtype)
  message(valid_msg)

  if (dsname == "core"){
    filenames <- core_file_constructor(time = time,
                                       scope.orgtype = scope.orgtype,
                                       scope.formtype = scope.formtype)
  }

  if (aws == FALSE){

    urls <- obj_validate(dsname = dsname,
                         filenames = filenames)

    # download all, filter, collapse

    # download

    load_df <- function(url){
      df <- readr::read_csv( url ) %>%
        data.table::as.data.table()
      return(df)
    }

    df_ls <- lapply(urls,
                    load_df)

    # collapse
    df_full <- data.table::rbindlist(df_ls,
                                     fill = TRUE)

  } else {

    # Check if files exist
    keys <- s3_validate(dsname = dsname,
                        filenames = filenames)

    # Query keys
    df_ls <- s3_query(bucket = "nccsdata",
                               keys = keys,
                               geo.state = geo.state,
                               ntee.cc = nteecc_matches)

    # collapse
    df_full <- data.table::rbindlist(df_ls,
                                     fill = TRUE)
  }

  # conditional filtering

  if (all(lengths(ntee, ntee.group, ntee.code, ntee.orgtype) > 0)){

    # NTEE parsing
    nteecc_matches <- nteecc_map(ntee.user = ntee,
                                 ntee.group = ntee.group,
                                 ntee.code = ntee.code,
                                 ntee.orgtype = ntee.orgtype)

    df_full <- df_full %>%
      dplyr::filter(.data$NTEECC %in% nteecc_matches)

  }

  if (all(lengths(geo.state, geo.city, geo.county) > 0)){

    # FIPS parsing
    fips_matches <- fips_map(geo.state,
                             geo.city,
                             geo.county)

    df_full <- df_full %>%
      dplyr::filter(.data$FIPS %in% fips_matches)

  }

  # Merge with ntee dataset

  # Merge with geo dataset

  return(df_full)

}


