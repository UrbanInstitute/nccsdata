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
                     scope.formtype = "PC",
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

  ntee2_matches <- query_ntee(ntee.user = ntee,
                              ntee.group = ntee.group,
                              ntee.code = ntee.code,
                              ntee.orgtype = ntee.orgtype)

  query_results <- query_s3(time = time,
                            scope.orgtype = scope.orgtype,
                            scope.formtype = scope.formtype,
                            geo.state = geo.state)
  return(query_results)

}

#' @title Function to download core data from S3 bucket.


dl_core <- function(filenames){

  base_url <- "https://nccsdata.s3.amazonaws.com/legacy/core/"

  urls <- paste0(base_url, filenames)
  valid_urls <- urls[RCurl::url.exists(urls)]

  return(valid_urls)

}

#' @title Function to perform S3 Select query on
#'
#' @import paws

query_s3 <- function(time,
                     scope.orgtype,
                     scope.formtype,
                     geo.state){

  core_files <- core_file_constructor(time = time,
                                      scope.orgtype = scope.orgtype,
                                      scope.formtype = scope.formtype)

  bucket_root <- "legacy/core/"
  keys <- paste0(bucket_root, core_files)
  bucket <- "nccsdata"

  s3 <- paws::s3()

  query <- "select * from s3object where STATE in (%s)"
  query <- sprintf(query, paste(sprintf("'%s'",geo.state), collapse=","))

  # Run a SQL query on data in a CSV in S3, and get the query's result set.
  result <- s3$select_object_content(
    Bucket = bucket,
    Key = keys[1],
    Expression = query,
    ExpressionType = "SQL",
    InputSerialization = list(
      'CSV' = list(
        FileHeaderInfo = "USE"
      )
    ),
    OutputSerialization = list(
      'CSV'= list(
        QuoteFields = "ASNEEDED"
      )
    )
  )

  # Convert the resulting CSV data into an R data frame.
  data <- read.csv(text = result$Payload$Records$Payload, header = FALSE)
  return(data)

}


