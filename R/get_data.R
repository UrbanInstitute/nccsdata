#' @title Download, filter and merge metadata for NCCS legacy core or bmf datasets
#'
#' @description This function uses user inputs to query, filter and merge nccs
#' data and additional census, cbsa and ntee metadata
#'
#' @param dsname character scalar. Name of data series to query from S3.
#' Valid inputs are either "core" or "bmf", not both.
#' @param time character vector. Dates of core/bmf files to query. Valid
#' inputs range from 1989-2022.
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
#' @param geo.region character vector. Regions for filtering e.g. "South",
#' "Midwest" based on census region classifications.
#' @param ntee character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
#' @param ntee.group character vector. Specific Industry Group codes submitted
#' by user
#' @param ntee.code character vector. Specific level 2-4 codes (Industry,
#' Division, Subdivision) submitted by user.
#' @param ntee.orgtype character vector. Specific level 5 codes (Organization
#' Type) submitted by user.
#' @param append.bmf boolean. Option to merge queried core data with bmf data.
#' Involves downloading the bmf dataset and will take longer. Default == FALSE.
#'
#' @return data.table with queried data
#'
#' @export

get_data <- function(dsname = NULL,
                     time = "2015",
                     scope.orgtype = "NONPROFIT",
                     scope.formtype = "PZ",
                     geo.state = NULL,
                     geo.city = NULL,
                     geo.county = NULL,
                     geo.region = NULL,
                     ntee = NULL,
                     ntee.group = NULL,
                     ntee.code = NULL,
                     ntee.orgtype = NULL,
                     append.bmf = FALSE){

  # Validate critical inputs with set acceptable entries
  message(validate_get_data(dsname = dsname,
                            time = time,
                            scope.orgtype = scope.orgtype,
                            scope.formtype = scope.formtype,
                            geo.state = geo.state,
                            geo.region = geo.region))

  # Create filters
  filter_ls <- list(nteecc_matches = nteecc_map(ntee.user = ntee,
                                                ntee.group = ntee.group,
                                                ntee.code = ntee.code,
                                                ntee.orgtype = ntee.orgtype),
                    state_matches = toupper(geo.state),
                    city_matches = toupper(geo.city),
                    county_fips_matches = fips_map(geo.region = firstupper(geo.region),
                                                   geo.county = geo.county))

  if (dsname == "core"){

    message("Downloading core data")

    core_dt <- get_core(dsname = dsname,
                        time = time,
                        scope.orgtype = scope.orgtype,
                        scope.formtype = scope.formtype,
                        filters = filter_ls,
                        append.bmf = append.bmf)

    message("Core data downloaded")

    if (append.bmf == TRUE){

      message("Downloading bmf data")

      bmf <- get_bmf(url = "https://nccsdata.s3.us-east-1.amazonaws.com/current/bmf/bmf-master.rds",
                     filters = filter_ls)

      message("bmf data downloaded. Appending bmf")

      core_dt <- bmf[core_dt, on = "EIN"]

      remove(bmf)

    }

    return(core_dt)

  } else if (dsname == "bmf"){

    response <- download_size(dsname = dsname,
                              append.bmf = append.bmf)

    message("Downloading bmf data")

    bmf <- get_bmf(url = "https://nccsdata.s3.us-east-1.amazonaws.com/current/bmf/bmf-master.rds",
                   filters = filter_ls)

    message("bmf data downloaded")

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
#' @param filters list. List of column filters to apply
#' @param append.bmf boolean. Option to merge queried core data with bmf data.
#' Involves downloading the bmf dataset and will take longer. Default == FALSE.
#'
#' @return a fully merged core data.table for the end user
#'
#'
#' @importFrom data.table rbindlist
#' @importFrom data.table setDT
#' @importFrom utils askYesNo

get_core <- function(dsname,
                     time,
                     scope.orgtype,
                     scope.formtype,
                     filters,
                     append.bmf){

  ntee_dat <- ntee_df %>%
    rename("NTEECC" = .data$old.code) %>%
    data.table::setDT()

  filenames <- core_file_constructor(time = time,
                                     scope.orgtype = scope.orgtype,
                                     scope.formtype = scope.formtype)

  urls <- obj_validate(dsname = dsname,
                       filenames = filenames)

  # Ask User for permission to perform downloads
  response <- download_size(dsname = dsname,
                            append.bmf = append.bmf,
                            urls = urls)


  dt <- lapply(urls, load_dt)
  dt <- data.table::rbindlist(dt, fill = TRUE)

  # Filter datasets
  dt <- filter_data(dt = dt, filters = filters)

  # Merge data
  dt <- ntee_dat[dt, on = "NTEECC"]

  return(dt)

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
#' @param filters list. List of column filters to apply
#'
#' @return data.table. Data.table with filtered master bmf file.
#'
#' @importFrom data.table setDT
#' @importFrom data.table setkey
#' @importFrom utils download.file
#' @importFrom data.table :=

get_bmf <- function(url,
                    dest_path = "bmf.rds",
                    filters){

  FIPS <- NULL # for global variable binding

  utils::download.file(url, destfile=dest_path)
  bmf <- readRDS(dest_path)
  file.remove(dest_path)

  data.table::setDT(bmf)
  bmf <- bmf[, FIPS := as.numeric(FIPS)]

  bmf <- filter_data(dt = bmf, filters = filters)

  return(bmf)

}
