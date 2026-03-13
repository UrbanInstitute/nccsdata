#' BMF Data Dictionary
#'
#' A tibble containing column names, descriptions, and types for the NCCS
#' Business Master File (BMF) parquet dataset.
#'
#' @format A tibble with 97 rows and 3 columns:
#' \describe{
#'   \item{column_name}{Column name in the BMF parquet file}
#'   \item{description}{Human-readable description of the column}
#'   \item{type}{Data type (character, integer, numeric, logical, IDate)}
#' }
#' @source NCCS S3 bucket data dictionary
"bmf_dictionary"

#' Browse the BMF Data Dictionary
#'
#' Returns the BMF data dictionary as a tibble, with optional grep filtering
#' on column names. Use this to discover available columns before calling
#' [nccs_read()].
#'
#' @param pattern Optional character string. If provided, filters the
#'   dictionary to column names matching this pattern (case-insensitive
#'   regular expression).
#'
#' @return A tibble with columns `column_name`, `description`, and `type`.
#'
#' @examples
#' # See all columns
#' nccs_dictionary()
#'
#' # Find geocoding-related columns
#' nccs_dictionary("geo")
#'
#' # Find NTEE-related columns
#' nccs_dictionary("ntee")
#'
#' @export
nccs_dictionary <- function(pattern = NULL) {
  dict <- nccsdata::bmf_dictionary

  if (!is.null(pattern)) {
    matches <- grepl(pattern, dict$column_name, ignore.case = TRUE)
    dict <- dict[matches, ]
  }

  dict
}
