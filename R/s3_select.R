#' @title Function to perform S3 Select query on s3 bucket
#'
#' @description This function queries multiple core/bmf .csv S3 objects and
#' returns a list of object subsets that satisfy user-specified filter
#' conditions.
#'
#' @param bucket character scalar. Name of s3 bucket to query
#' @param keys character vector. s3 object keys
#' @param filters list. List of column filters to apply
#'
#' @return list of queried dataframes. One for each key supplied.
#'
#' @usage s3_query(bucket, keys, filters)
#'
#' @importFrom purrr map2

s3_query <- function(bucket,
                     keys,
                     filters){

  header_query <- "SELECT * FROM s3object s LIMIT 1"
  query <- query_construct(filters = filters)

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
#' @description This function takes in a list of filter arguments from
#' get_data() and constructs a SQL query for use in s3 select
#'
#' @param filters list. List of column filters to apply
#'
#' @return a completed SQL query
#'
#' @usage query_construct(filters)
#'
#' @importFrom rlang is_empty

query_construct <- function(filters){

  full_query <- "SELECT * FROM S3Object"
  filter_map <- list(nteecc_matches = "NTEECC",
                     state_matches = "STATE",
                     city_matches = "CITY",
                     county_fips_matches = "FIPS")

  for (filter in names(filters)){

    if (! rlang::is_empty(filters[[filter]])){

      colname <- filter_map[[filter]]
      colvalues <- filters[[filter]]

      sub_query <- ifelse(grepl("WHERE", full_query),
                          sprintf(" AND %s IN", colname),
                          sprintf(" WHERE %s IN", colname))

      full_query <- paste0(full_query, sub_query)

      sub_query <- " (%s)"

      sub_query <- sprintf(sub_query,
                           paste(sprintf("'%s'", colvalues),
                                 collapse = ","))

      full_query <- paste0(full_query, sub_query)

    }

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
#' @importFrom utils read.csv
#' @importFrom paws s3

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
