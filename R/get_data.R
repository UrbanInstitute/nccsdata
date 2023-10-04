#' @title Function to query bmf data and integrate it with census, cbsa and ntee
#' metadata
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
                     ntee.orgtype = NULL){

  # Validate inputs
  message(validate_get_data(dsname = dsname,
                            time = time,
                            scope.orgtype = scope.orgtype,
                            scope.formtype = scope.formtype))

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

    core_dt <- get_core(dsname = dsname,
                        time = time,
                        scope.orgtype = scope.orgtype,
                        scope.formtype = scope.formtype,
                        filters = filter_ls)

    return(core_dt)

  } else if (dsname == "bmf"){

    bmf <- get_bmf(url = "https://nccsdata.s3.us-east-1.amazonaws.com/current/bmf/bmf-master.rds",
                   filters = filter_ls)

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
#'
#' @return a fully merged core data.table for the end user
#'
#' @usage get_core(dsname,time, scope.orgtype, scope.formtype,filters, aws)
#'
#' @importFrom data.table rbindlist
#' @importFrom data.table setDT
#' @importFrom utils askYesNo

get_core <- function(dsname,
                     time,
                     scope.orgtype,
                     scope.formtype,
                     filters){

  filenames <- core_file_constructor(time = time,
                                     scope.orgtype = scope.orgtype,
                                     scope.formtype = scope.formtype)

  ntee_dat <- ntee_df %>%
    rename("NTEECC" = .data$old.code) %>%
    data.table::setDT()

    # Download datasets to disk
  urls <- obj_validate(dsname = dsname,
                       filenames = filenames)

  size_mb <- Reduce("+", s3_size_dic[urls]) / 1000000

  prompt <- sprintf("Requested files have a total size of %s MB. Proceed
                      with download? Enter Y/N",
                    round(size_mb, 1))

  response <- utils::askYesNo(msg = prompt,
                              default = FALSE)

    if (response == TRUE){

      dt_ls <- lapply(urls, load_dt)
      dt_full <- data.table::rbindlist(dt_ls,
                                       fill = TRUE)

      # Filter datasets
      dt_filtered <- filter_data(dt = dt_full,
                                 filters = filters)

      remove(dt_ls)
    } else {

      return(message("Download aborted."))

    }

  # Merge data
  dt_merged <- ntee_dat[dt_filtered, on = "NTEECC"]
  remove(dt_filtered)


  return(dt_merged)

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

  bmf_filtered <- filter_data(dt = bmf,
                              filters = filters)

  remove(bmf)

  return(bmf_filtered)

}
