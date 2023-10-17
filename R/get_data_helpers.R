#' @title Function to validate inputs to get_data()
#'
#' @description This function validates each argument passed to get_data() and
#' returns informative error messages if user inputs are incorrect.
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
#' @param geo.region character vector. Regions for filtering e.g. "South",
#' "Midwest" based on census region classifications.
#'
#' @return message describing the data that is being queried.
#'
#' @importFrom rlang is_scalar_character
#' @importFrom rlang is_character

validate_get_data <- function(dsname,
                              time,
                              scope.orgtype,
                              scope.formtype,
                              geo.state,
                              geo.region){

  stopifnot(

    "Invalid datatype. dsname must be a string." =
      rlang::is_scalar_character(dsname) == TRUE,

    "Invalid dataseries. Select either 'core' or 'bmf'." =
      (dsname == "core" | dsname == "bmf"),

    "Invalid datatype. time must be a character vector." =
      rlang::is_character(time) == TRUE,

    "Invalid date range specified. Choose years from 1989-2022." =
      all(as.numeric(time) %in% c(seq(1989, 2022), "current")),

    "Invalid datatype. scope.orgtype must be a string." =
      rlang::is_scalar_character(scope.orgtype) == TRUE,

    "Invalid organisation type. Select 'CHARITIES' for charities (501C3-PC),
    'PRIVFOUND' for private foundations (501C3-PF) and
    'NONPROFIT' for all nonprofits (501CE)" =
      (scope.orgtype == "CHARITIES" |
       scope.orgtype == "PRIVFOUND" |
       scope.orgtype == "NONPROFIT"),

    "Invalid datatype. scope.formtype must be a string." =
      rlang::is_scalar_character(scope.formtype) == TRUE,

    "Invalid formtype. Select 'PC'(nonprofits that file the full version),
    'EZ'(nonprofits that file 990EZs only),
    'PZ'(nonprofits that file both PC and EZ),
    or 'PF'(private foundations)" =
      (scope.formtype == "PC" |
       scope.formtype == "EZ" |
       scope.formtype == "PZ" |
       scope.formtype == "PF"),

    "Invalid geo.state. Use State Abbreviations instead. e.g. 'NY' for New York,
    'CA' for California etc." =
      geo.state %in% cbsa_df$state.census.abbr,

    "Invalid geo.region. Only the following regions are accepted:
    'South', 'West', 'Midwest', 'Northeast'" =
      geo.region %in% tract_dat$region.census.main

    )

  return("Valid inputs detected. Retrieving data.")
}


#' @title function to construct filenames from aws s3 core bucket
#'
#' @description This function constructs names for .csv files found in the
#' nccsdata bucket for nccs core files based on user inputs
#'
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
#'
#' @return character vector. Vector of .csv filenames to query in s3 bucket.
#'
#' @usage core_file_constructor(time, scope.orgtype, scope.formtype)

core_file_constructor <- function(time,
                                  scope.orgtype,
                                  scope.formtype){
  # Organization Type dictionary
  orgtype_dic <- c("CHARITIES" = "CHARITIES-SCOPE-501C3",
                   "PRIVFOUND" = "PRIVFOUND-SCOPE-501C3",
                   "NONPROFIT" = "NONPROFIT-SCOPE-501CE")

  # Base names
  root_file <- "CORE-"
  file_ext <- ".csv"

  # Add years
  files_year <- paste0(root_file, time)

  # Add orgtype
  orgtype_full <- orgtype_dic[scope.orgtype]
  files_orgtype <- as.vector(outer(files_year,
                                   orgtype_full,
                                   paste,
                                   sep = "-"))

  # Add formtype
  files_formtype <- paste(files_orgtype,
                          scope.formtype,
                          sep = "-")

  # Add extension
  files_fullname <- paste(files_formtype,
                          file_ext,
                          sep = "")

  return(files_fullname)

}


#' @title Function to filter a data.table with user-provided arguments
#'
#' @description This function takes a list of filters and filters either the
#' core or bmf data.table objects.
#'
#' @param dt data.table. Data.table to filter
#' @param filters list. List of column filters to apply
#'
#' @usage filter_data(dt, filters)
#'
#' @returns filtered data.table

filter_data <- function(dt,
                        filters){

  filter_map <- list(nteecc_matches = "NTEECC",
                     state_matches = "STATE",
                     city_matches = "CITY",
                     county_fips_matches = "FIPS")

  for (filter in names(filters)){

    if (! rlang::is_empty(filters[[filter]])){

      col_name <- as.name(filter_map[[filter]])

      data.table::setkeyv(dt, filter_map[[filter]])
      dt <- dt[eval(col_name) %in% filters[[filter]], ]

    }

  }

  return(dt)

}


#' @title Data size calculator.
#'
#' @description This function calculates the size of required downloads
#' for a user's query.
#'
#' @param dsname character scalar. Name of data series to query from S3.
#' Valid inputs are either "core" or "bmf", not both.
#' @param append.bmf boolean. Option to merge queried core data with bmf data.
#' Involves downloading the bmf dataset and will take longer.
#' @param urls character vector. Vector of urls to s3 buckets for download.
#'
#' @returns a response by the user indicating whether they want to proceed with
#' the download
#'
#' @importFrom utils askYesNo

download_size <- function(dsname,
                          append.bmf,
                          urls = NULL){


  if (dsname == "core"){

    size_mb <- Reduce("+", s3_size_dic[urls]) / 1000000

    if (append.bmf == TRUE){

      size_mb <- size_mb + 190.0

    }

  } else if (dsname == "bmf"){

    size_mb <- 190.0

  }

  prompt <- sprintf("Requested files have a total size of %s MB. Proceed
                      with download? Enter Y/N",
                    round(size_mb, 1))

  response <- utils::askYesNo(msg = prompt,
                              default = TRUE)

  stopifnot("Download Aborted" = response == TRUE)

  return(response)

}
