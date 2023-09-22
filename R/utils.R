#' Script with utility functions

#' Function that prettifies json
#'
#' @param f json string. String containing json text
#'
#' @usage jsonify_f(f)
#'
#' @import jsonlite

jsonify_f <- function(f)
{
  f <- as.factor(f)
  f_level <- levels(f)
  f_level <- gsub( "'", "", f_level ) # remove quotes
  f_level <- gsub( '"', '', f_level ) # remove apostrophes
  label <- f_level
  d <- data.frame(f_level,label)
  jd <- jsonlite::toJSON( d )
  jd <- gsub( "\\{", "  \\{  ", jd )
  jd <- gsub( ":", " :  ", jd )
  jd <- gsub( '",', '"  ,  ', jd )
  jd <- gsub( '","', '",  "', jd )
  jd <- gsub( "\\},", "  \\}, \n", jd )
  jd <- gsub( "\\[", "\\[ \n", jd )
  jd <- gsub( "\\]", "\n\\]", jd )
  jd <- gsub( "\\}\n", "  \\}\n", jd )
  return(jd)
}

#' Check if ALL objects named one vector exist in memory
#'
#' @description This function is used to check if both block and tract data
#' tables exist in memory
#'
#' @param ... vector of objects.
#'
#' @usage objs_exist(...)
#'
#' @return A single boolean value indicating whether all objects are present
#' in memory

objs_exist <- function(...) {

  ls <- list(...)
  all(sapply(ls, exists))

}


#' @title Function to validate constructed file names and return s3 keys
#'
#' @description This function takes constructed file names from
#' core_file_constructor() and tests if the urls to those files exist. Then it
#' extracts s3 bucket keys from the validated urls for downstream s3 queries or
#' returns the validated urls themselves for local download.
#'
#' @param dsname character scalar. Name of data series to query from S3.
#' Valid inputs are "core" and "bmf", not both.
#' @param filenames character vector. Vector of file names returned by
#' core_file_constructor
#' @param bucket.str character scalar. Url of legacy core/bmf bucket for
#' string formatting
#' @param base.url character scalar. Base url of nccsdata s3 bucket.
#' @param return.key boolean. Default == FALSE to return url links to s3
#' buckets. Default == TRUE returns bucket keys for s3_select queries.
#'
#' @returns vector of valid s3 bucket keys for core data sets
#'
#' @usage obj_validate(dsname, filenames)
#'
#' @importFrom RCurl url.exists


obj_validate <- function(dsname,
                         filenames,
                         bucket.str = "https://nccsdata.s3.amazonaws.com/legacy/%s/",
                         base.url = "https://nccsdata.s3.amazonaws.com/",
                         return.key = FALSE){

  base_bucket <- sprintf(bucket.str, dsname)

  urls <- paste0(base_bucket, filenames)
  valid_urls <- urls[RCurl::url.exists(urls)]
  valid_keys <- gsub(base.url, "", valid_urls)

  ifelse(return.key == FALSE,
         return(valid_urls),
         return(valid_keys))

}

#' @title Function to read csv files from a public s3 bucket url. Saves the
#' file as a data.table.
#'
#' @description This function is used in lapply to map a list of urls to a
#' list of data.tables.
#'
#' @param url character scalar. Link to public s3 object.
#'
#' @returns data.table
#'
#' @usage load_df(url)
#'
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @importFrom data.table as.data.table
#' @importFrom dplyr %>%

load_dt <- function(url){
  df <- readr::read_csv(
    url,
    col_types = readr::cols(default = "?",
                            FIPS = "i")
  ) %>%
    data.table::as.data.table()
  return(dt)
}
