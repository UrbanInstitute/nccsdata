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
#'
#' @return message describing the data that is being queried.
#'
#' @usage validate_get_data(dsname, time, scope.orgtype, scope.formtype)
#'
#' @export
validate_get_data <- function(dsname = NULL,
                              time = NULL,
                              scope.orgtype = NULL,
                              scope.formtype = NULL){

  stopifnot(

    "Invalid datatype. dsname must be a string." =
      is_scalar_character(dsname) == TRUE,

    "Invalid dataseries. Select either 'core' or 'bmf'." =
      (dsname == "core" | dsname == "bmf"),

    "Invalid datatype. time must be a character vector." =
      is_character(time) == TRUE,

    "Invalid date range specified. Choose years from 1989-2022." =
      all(as.numeric(time) %in% c(seq(1989, 2022), "current")),

    "Invalid datatype. scope.orgtype must be a string." =
      is_scalar_character(scope.orgtype) == TRUE,

    "Invalid organisation type. Select 'CHARITIES' for charities (501C3-PC), 'PRIVFOUND' for private foundations (501C3-PF) and 'NONPROFIT' for all nonprofits (501CE)" =
      (scope.orgtype == "CHARITIES" |
       scope.orgtype == "PRIVFOUND" |
       scope.orgtype == "NONPROFIT"),

    "Invalid datatype. scope.formtype must be a string." =
      is_scalar_character(scope.formtype) == TRUE,

    "Invalid formtype. Select 'PC'(nonprofits that file the full version), 'EZ'(nonprofits that file 990EZs only), 'PZ'(nonprofits that file both PC and EZ), or 'PF'(private foundations)" =
      (scope.formtype == "PC" |
       scope.formtype == "EZ" |
       scope.formtype == "PZ" |
       scope.formtype == "PF")
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
#'
#' @export
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

#' @title Function to validate constructed file names and return s3 keys
#'
#' @description This function takes constructed file names from
#' core_file_constructor() and tests if the urls to those files exist. Then it
#' extracts s3 bucket keys from the validated urls for downstream s3 queries.
#'
#' @param filenames character vector. Vector of file names returned by
#' core_file_constructor
#'
#' @returns vector of valid s3 bucket keys for core data sets
#'
#' @usage core_validate(filenames)
#'
#' @importFrom RCurl url.exists


s3_validate <- function(dsname,
                        filenames){

  base_bucket <- sprintf("https://nccsdata.s3.amazonaws.com/legacy/%s/", dsname)
  base_url <- "https://nccsdata.s3.amazonaws.com/"

  urls <- paste0(base_bucket, filenames)
  valid_urls <- urls[RCurl::url.exists(urls)]
  valid_keys <- gsub(base_url, "", valid_urls)

  return(valid_keys)

}

#' @title Function to perform S3 Select query on s3 bucket
#'
#' @description This function queries multiple core/bmf .csv S3 objects and
#' returns a list of object subsets that satisfy user-specified filter
#' conditions.
#'
#' @param bucket character scalar. Name of s3 bucket to query
#' @param keys character vector. s3 object keys
#' @param geo.state character vector. Vector of state abbreviations
#' @param ntee character vector. Vector of ntee codes found in NTEECC
#'
#' @return list of queried dataframes. One for each key supplied.
#'
#' @usage s3_query(bucket, keys, geo.state, ntee)
#'
#' @importFrom purrr map2

s3_query <- function(bucket,
                     keys,
                     geo.state,
                     ntee.cc){

  header_query <- "SELECT * FROM s3object s LIMIT 1"
  query <- query_construct(geo.state = geo.state,
                           ntee.cc = ntee.cc)

  # Execute queries
  df_headers_ls <- lapply(keys,
                          paws_s3_select,
                          bucket = bucket,
                          query = header_query,
                          file.header = "NONE",
                          csv.header = TRUE)
  df_headers_ls <- lapply(df_headers_ls,
                          colnames)

  df_body_ls <- lapply(keys,
                       paws_s3_select,
                       bucket = bucket,
                       query = query,
                       file.header = "USE",
                       csv.header = FALSE)

  df_full <- purrr::map2(.x = df_body_ls,
                         .y = df_headers_ls,
                         .f = function(x, y){
                           colnames(x) <- y
                           return(x)
                           })

  return(df_full)

}


#' @title function to construct SQL queries for s3 select
#'
#' @description This function takes in filter arguments from get_data() and
#' constructs a SQL query for s3 select
#'
#' @param geo.state character vector. Vector of state abbreviations
#' @param ntee character vector. Vector of ntee codes found in NTEECC
#'
#' @return a completed SQL query
#'
#' @usage query_construct(geo.state, ntee)

query_construct <- function(geo.state,
                            ntee.cc){

  full_query <- "select * from s3object"

  if (is.null(geo.state) == FALSE){
    sub_query <- " where STATE in (%s)"
    geo_query <- sprintf(sub_query,
                         paste(sprintf("'%s'", geo.state),
                               collapse=","))
    full_query <- paste0(full_query, geo_query)
  }
  # add where/and selection
  if (is.null(ntee.cc) == FALSE){
    sub_query <- " and NTEECC in (%s)"
    ntee_query <- sprintf(sub_query,
                          paste(sprintf("'%s'", ntee.cc),
                                collapse=","))
    full_query <- paste0(full_query, ntee_query)
  }

  return(full_query)
}


#' @title function to perform s3_select with paws
#'
#' @description This function uses paws to perform aws s3 select queries on a
#' user provided object key and bucket.
#'
#' @param bucket character scalar. Name of s3 bucket.
#' @param key character scalar. s3 object key.
#' @param query character scalar. Desired SQL query.
#' @param file.header character scalar. Option to use header columns in SQL
#' query. Acceptable inputs are USE | NONE | IGNORE
#' @param csv.header boolean. Option to return csv with first row as header.
#'
#' @return dataframe with query results
#'
#' @usage paws_s3_select(bucket, key, query, file.header, csv.header)
#'
#' @importFrom paws s3
#' @importFrom utils read.csv

paws_s3_select <- function(bucket,
                           key,
                           query,
                           file.header,
                           csv.header){

  s3 <- paws::s3()

  # Run a SQL query on data in a CSV in S3, and get the query's result set.
  result <- s3$select_object_content(
    Bucket = bucket,
    Key = key,
    Expression = query,
    ExpressionType = "SQL",
    InputSerialization = list(
      'CSV' = list(
        FileHeaderInfo = file.header,
        AllowQuotedRecordDelimiter = TRUE
      )
    ),
    OutputSerialization = list(
      'CSV'= list(
        QuoteFields = "ASNEEDED"
      )
    )
  )

  # Convert the resulting CSV data into an R data frame.
  data <- utils::read.csv(text = result$Payload$Records$Payload,
                          header = csv.header)

  return(data)
}
