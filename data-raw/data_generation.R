# Script with functions that generate data in internal storage (R/sysdata.rda)

#' Preprocess Census Block and Tract Crosswalks
#'
#' @description This function downloads the crosswalk datasets from an S3
#' bucket, reads them as data.tables, reformats the column names, and creates
#' a new column with state abbreviations for the Tract dataset.
#'
#' @param block_s3_url string. Path to S3 bucket with block crosswalk.
#' @param tract_s3_url string. Path to S3 bucket with tract crosswalk.
#'
#' @usage geo_data_get(block_s3_url, tract_s3_url)
#'
#' @return A string message indicating that the processed data.tables
#' are available in memory
#'
#' @note Can also be used to recreate the data.tables if files get corrupted.
#'
#' @importFrom data.table fread
#' @importFrom rlang .data
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom usdata state2abbr
#' @importFrom aws.s3 save_object

geo_data_get <- function(
    block_s3_url = "s3://nccsdata/geo/xwalk/BLOCKX.csv",
    tract_s3_url = "s3://nccsdata/geo/xwalk/TRACTX.csv"
){

  message("Loading Datasets")

  # Load Data from S3

  block_dat <- aws.s3::save_object(block_s3_url) %>%
    data.table::fread()

  tract_dat <- aws.s3::save_object(tract_s3_url) %>%
    data.table::fread()

  # Set global variables
  utils::globalVariables(names(tract_dat))
  utils::globalVariables(names(block_dat))

  # Wrangle data in tract dataset
  tract_dat <- tract_dat %>%
    dplyr::rename("metro.census.cbsa.geoid" = .data$metro.census.cbsa10.geoid,
                  "metro.census.cbsa.name" = .data$metro.census.cbsa10.name,
                  "metro.census.csa.geoid" = .data$metro.census.csa10.geoid,
                  "metro.census.csa.name" = .data$metro.census.csa10.name) %>%
    dplyr::mutate("state.census.abbr" = usdata::state2abbr(.data$state.census.name),
                  "tract.census.geoid" = as.character(
                    as.numeric(.data$tract.census.geoid)
                  ))

  # wrangle data in block dataset
  block_dat <- block_dat %>%
    dplyr::mutate("block.census.geoid" = as.character(
      as.numeric(.data$block.census.geoid)
    ))

  message("Census and Block datasets downloaded and processed.")

  return(list(tract_dat, block_dat))

}

#' This function preprocesses the disaggregated NTEE dataset and returns
#' a vector of NTEE codes formatted according to NTEE2 standards.
#'
#' @description This function reads in a csv containing disaggregated NTEE
#' codes built from the old formatting. It uses the rules specified in
#' https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md
#' to derive the codes from level 1-5 used in NTEE2 from old NTEE codes
#'
#' @param path_to_csv character. Path to the .csv file with disaggregated NTEE
#' data
#'
#' @usage ntee_preproc(path_to_csv)
#'
#' @return A list containing the population of valid NTEE2 codes
#'
#' @note The disaggregated csv file can be edited and the function rerun
#' to generate a new list of valid NTEE2 codes if needed.
#'
#' @import dplyr
#' @import data.table
#' @importFrom utils read.csv

ntee_preproc <- function(path_to_csv = "data-raw/ntee-disaggregated.csv"){

  # Read csv file to extract different versions of NTEE Codes
  ntee_df <- utils::read.csv(path_to_csv)

  # Extract level 1 and level 2 parts of NTEE2 Code
  ntee2_level1 <- ntee_df$broad.category
  ntee2_level2 <- ntee_df$major.group

  # Extract digits23 and digits 45
  digits23 <- substring(ntee_df$old.code, 2, 3)
  digits45 <- substring(ntee_df$old.code, 4, 5)
  digits45 <- replace(digits45, digits45 == "", "00")

  # Use digits23 and digits45 to get level 3 and 4 of new NTEE code
  ntee2_level_3_4 <- mapply(get_ntee_level_3_4, digits23, digits45)

  # Combine levels 2-4
  ntee2_level_2_4 <- paste(ntee2_level2, ntee2_level_3_4, sep = "")

  # Extract level 5 code from disaggregated csv
  ntee2_level5 <- ntee_df$type.org

  # Create population of NTEE2 codes
  ntee_new_codes <- paste(
    ntee2_level1,
    "-",
    ntee2_level2,
    ntee2_level_3_4,
    "-",
    ntee2_level5,
    sep = ""
  )

  # Append NTEE2 codes to disaggregated csv and save
  ntee_df$ntee2.code <- ntee_new_codes
  usethis::use_data(ntee_df, internal = TRUE)

  message("CSV has been preprocessed and stored internally")

  return(list(ntee_new_codes,
              ntee2_level1,
              ntee2_level_2_4,
              ntee2_level5))
}

# Function to generate size dictionary

s3_metadata <- read.csv("data-raw/AWS-NCCSDATA.csv")
s3_metadata$url <- paste0("https://nccsdata.s3.amazonaws.com/",
                          s3_metadata$Key)

s3_size_dic <- dic_from_df(df = s3_metadata,
                           keycol = "url",
                           valcol = "Size")
