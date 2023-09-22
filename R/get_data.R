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
#' @importFrom data.table setDT
#' @importFrom data.table rbindlist
#' @importFrom rlang .data
#' @importFrom dplyr filter
#' @importFrom dplyr %>%

get_data <- function(dsname = NULL,
                     time = "2015",
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
  message(validate_get_data(dsname = dsname,
                            time = time,
                            scope.orgtype = scope.orgtype,
                            scope.formtype = scope.formtype))

  # Get filters

  if (! is.null(c(ntee, ntee.group, ntee.code, ntee.orgtype))){

    # NTEE parsing
    nteecc_matches <- nteecc_map(ntee.user = ntee,
                                 ntee.group = ntee.group,
                                 ntee.code = ntee.code,
                                 ntee.orgtype = ntee.orgtype)

  } else {
    nteecc_matches <- NULL
  }

  if (! is.null(c(geo.state, geo.city, geo.county))){

    # FIPS parsing
    fips_matches <- fips_map(geo.state,
                             geo.city,
                             geo.county)

  } else {
    fips_matches <- NULL
  }

  if (dsname == "core"){

    core_dt <- get_core(dsname = dsname,
                        time = time,
                        scope.orgtype = scope.orgtype,
                        scope.formtype = scope.formtype,
                        ntee_matches = nteecc_matches,
                        fips_matches = fips_matches,
                        aws = aws)

    return(core_dt)

  } else if (dsname == "bmf"){

    bmf <- get_bmf(url = "https://nccsdata.s3.us-east-1.amazonaws.com/current/bmf/bmf-master.rds",
                   ntee_matches = nteecc_matches,
                   fips_matches = fips_matches)

    return(bmf)

  }

  return(message("No data selected"))

}


#' @title Function to get core dataset.
#'
#' @description This function executes either the s3_select query or data
#' download and local merge on a specified subset of the core dataset. It then
#' merges the dataset with the ntee dataframe.
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
#' @param ntee_matches character vector. Vector of nteecc codes returned from
#'nteecc_map()
#' @param fips_matches numeric vector. Vector of fips codes returned from
#' fips_map()
#' @param aws boolean. Whether to use aws.s3::s3_select() in executing queries.
#' Default == FALSE, select TRUE to use s3_select. Must have aws account to use.
#'
#' @return a fully merged core data.table for the end user
#'
#' @usage get_core(dsname,time, scope.orgtype, scope.formtype,ntee_matches,
#' fips_matches, aws)

get_core <- function(dsname,
                     time,
                     scope.orgtype,
                     scope.formtype,
                     ntee_matches,
                     fips_matches,
                     aws){

  filenames <- core_file_constructor(time = time,
                                     scope.orgtype = scope.orgtype,
                                     scope.formtype = scope.formtype)

  ntee_dat <- ntee_df %>%
    rename("NTEECC" = .data$old.code) %>%
    data.table::setDT()

  if (aws == FALSE){

    urls <- obj_validate(dsname = dsname,filenames = filenames)
    dt_ls <- lapply(urls, load_dt)
    dt_full <- data.table::rbindlist(dt_ls, fill = TRUE)

    if (! is.null(ntee_matches)){

      data.table::setkey(dt_full, NTEECC)
      dt_full <- dt_full[NTEECC %in% ntee_matches, ]

    }

    dt_full <- dt_full[ntee_dat, on = "NTEECC"]

    if (! is.null(fips_matches)){

      data.table::setkey(dt_full, FIPS)
      dt_full <- dt_full[FIPS %in% fips_matches, ]

    }

  } else {

    # Must be character for SQL Query
    fips_matches <- ifelse(nchar(fips_matches == 4),
                           paste0("0", fips_matches),
                           fips_matches)

    keys <- obj_validate(dsname = dsname,
                         filenames = filenames,
                         return.key = TRUE)

    dt_ls <- s3_query(bucket = "nccsdata",
                      keys = keys,
                      ntee.cc = ntee_matches,
                      fips = fips_matches)

    dt_full <- data.table::rbindlist(dt_ls, fill = TRUE)

    dt_full <- dt_full[ntee_dat, on = "NTEECC"]

  }

  remove(dt_ls)
  return(dt_full)

}


#' @title Function to download master bmf file and filter it based on ntee
#' and FIPS codes
#'
#' @description This function downloads an .rds file from a public s3 bucket,
#' reads it into memory, and deletes the file. It then converts the data.frame
#' into a data.table and filters it based on user-specified FIPS codes and
#' ntee codes.
#'
#' @param url character scalar. Link to object in s3 bucket.
#' @param dest_path character scalar. Path to download bmf file to.
#' @param ntee_matches character vector. Vector of nteecc codes returned from
#'nteecc_map()
#' @param fips_matches numeric vector. Vector of fips codes returned from
#' fips_map()
#'
#' @return data.table. Data.table with filtered master bmf file.
#'
#' @importFrom data.table setDT
#' @importFrom data.table setkey

get_bmf <- function(url,
                    dest_path = "bmf.rds",
                    ntee_matches,
                    fips_matches){

  download.file(url, destfile=dest_path)
  bmf <- readRDS(dest_path)
  file.remove(dest_path)

  data.table::setDT(bmf)
  bmf <- bmf[, FIPS:=as.numeric(FIPS)]

  if (! is.null(fips_matches)){
    data.table::setkey(dt_full, FIPS)
    bmf <- bmf[FIPS %in% fips_matches, ]
  }

  if (! is.null(nteecc_matches)){
    data.table::setkey(dt_full, NTEECC)
    bmf <- bmf[NTEECC %in% nteecc_matches, ]
  }

  return(bmf)

}
