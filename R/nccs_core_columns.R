#' Column Dictionary for a CORE Series Partition
#'
#' Reads the per-partition column dictionary that accompanies every
#' published CORE Series file. One row per column in the matching data
#' file, with the harmonized name, source variable + location, dtype,
#' null/non-null counts, and the vintages where that column was
#' populated.
#'
#' Use this as the source of truth for "what columns exist in this
#' partition." Column lists evolve across vintages, so do not hardcode.
#'
#' @inheritParams nccs_core_url
#' @param pattern Optional case-insensitive regex applied to
#'   `harmonized_name` and `description`. `NULL` (default) returns the
#'   full dictionary.
#'
#' @return A tibble with one row per column in the partition.
#'
#' @seealso [nccs_core_url()], [nccs_read_core()].
#'
#' @examples
#' \dontrun{
#' # Full dictionary for the 2020 merged 990combined partition
#' nccs_core_columns("merged", 2020, "990combined")
#'
#' # Filter to revenue-related columns
#' nccs_core_columns("merged", 2020, "990combined", pattern = "revenue")
#' }
#'
#' @export
nccs_core_columns <- function(tier = c("merged", "soi", "legacy"),
                              tax_year,
                              form,
                              pattern = NULL) {
  tier <- match.arg(tier)
  .validate_core_partition(tax_year, form, tier)

  if (!is.null(pattern) &&
      (!is.character(pattern) || length(pattern) != 1L ||
       is.na(pattern))) {
    stop("`pattern` must be a single string or NULL.", call. = FALSE)
  }

  uri <- nccs_core_url(tier, tax_year, form,
                       format = "parquet", kind = "dictionary")
  dict <- dplyr::as_tibble(arrow::read_parquet(uri))

  if (!is.null(pattern)) {
    hit_name <- grepl(pattern, dict$harmonized_name, ignore.case = TRUE)
    hit_desc <- if ("description" %in% names(dict)) {
      grepl(pattern, dict$description, ignore.case = TRUE)
    } else {
      rep(FALSE, nrow(dict))
    }
    dict <- dict[hit_name | hit_desc, , drop = FALSE]
  }
  dict
}
