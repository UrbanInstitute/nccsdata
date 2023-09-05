#' Preprocess Census Block and Tract Crosswalks
#'
#' @description This function downloads the crosswalk datasets from an S3
#' bucket, reads them as data.tables, reformats the column names, and creates
#' a new column with state abbreviations for the Tract dataset.
#'
#' @param block_s3_url string. Path to S3 bucket with block crosswalk.
#' @param tract_s3_url string. Path to S3 bucket with tract crosswalk.
#'
#' @usage geo_data_get()
#'
#' @return A string message indicating that the processed data.tables
#' are available in memory
#'
#' @note Can also be used to recreate the data.tables if files get corrupted.
#'
#' @import data.table
#' @import dplyr
#' @import aws.s3

geo_data_get <- function(
    block_s3_url = "s3://nccsdata/geo/xwalk/BLOCKX.csv",
    tract_s3_url = "s3://nccsdata/geo/xwalk/TRACTX.csv"
){

  message("Loading Datasets")

  # Load Data from S3

  block_dat <- aws.s3::save_object(block_s3_url) %>%
    data.table::fread()

  tract_dat <- save_object(tract_s3_url) %>%
    data.table::fread()

  # Wrangle data in tract dataset
  tract_dat <- tract_dat %>%
    dplyr::rename(metro.census.cbsa.geoid = metro.census.cbsa10.geoid,
                  metro.census.cbsa.name = metro.census.cbsa10.name,
                  metro.census.csa.geoid = metro.census.csa10.geoid,
                  metro.census.csa.name = metro.census.csa10.name) %>%
    dplyr::mutate(state.census.abbr = usdata::state2abbr(state.census.name),
                  tract.census.geoid = as.character(
                                       as.numeric(tract.census.geoid)
                                       )
                  )

  # wrangle data in block dataset
  block_dat <- block_dat %>%
    dplyr::mutate(block.census.geoid = as.character(
                                       as.numeric(block.census.geoid)
                                       )
                  )


  # Save data as rda
  save(tract_dat, file = "data/tract_dat.rda")
  save(block_dat, file = "data/block_dat.rda")

  return("Data saved to disk")

}

#' Function to filter data table
#'
#' @description dat_filter() takes in arguments specified by the user (name of
#' dataset, column names and subsets), checks if those filters apply to the
#' specified dataset and returns FIPs codes that meet the subsetting conditions.
#'
#' A user specifies whether they are pulling data from the Block or Tract
#' datasets and specifies the subset of the data they want e.g. (geo.state.abbr
#' = c("NY")). The function returns either the Tract or Block IDs that fit
#' the subset specified
#'
#' @param dat data table. Data table of either census or block data.
#' @param args string expression. User arguments specified in parse_geo()
#' @param exp_args string expression. Processed user arguments specified in
#' parse_geo()
#' @param id_col string. Name of column with Tract or Block IDs
#' @param geo.level name of data.table (Block/Census) for error messages.
#'
#' @returns A list of fips codes based on filter criteria.
#'
#' @import dplyr

dat_filter <- function(dat,
                       args = args,
                       ex_args = ex_args,
                       id_col,
                       census.level){

  if (all(names(args) %in% colnames(dat))){

    parsed_ids <- dat %>%
      suppressWarnings(dplyr::filter(!!! ex_args)) %>%
      dplyr::select(dplyr::all_of(id_col))

    return(parsed_ids)

  } else {

    absent_colnames <- setdiff(names(args), colnames(dat))

    stop(paste("The following columns are not present in the",
               census.level,
               "dataset:",
               setdiff(names(args), colnames(dat))))
  }
}
