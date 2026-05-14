#' S3 URL of a Historical BMF Vintage
#'
#' Returns the S3 URI for a specific dated monthly BMF snapshot or its
#' per-vintage data dictionary. These artifacts are CSVs with
#' per-vintage schemas (no central crosswalk) and are *not* geocoded —
#' they carry raw Form 990 address columns instead of the master's
#' `geo_*` columns. Use this helper to fetch a URI, then read the file
#' yourself with `arrow::read_csv_arrow()` or `utils::read.csv()`.
#'
#' For analyses that don't require a specific vintage, prefer
#' [nccs_read()], which reads the rolling geocoded master parquet with
#' Arrow predicate-pushdown filters.
#'
#' @param vintage Character string of the form `"YYYY_MM"` (e.g.,
#'   `"2023_07"`).
#' @param kind One of `"data"` (default) for the BMF snapshot CSV or
#'   `"dictionary"` for the column data dictionary CSV.
#' @param legacy Logical. If `TRUE`, returns the legacy-archive path
#'   used for older vintages (`processed/bmf-legacy/...`). Defaults to
#'   `FALSE` (the modern `processed/bmf/...` path).
#'
#' @return A single character string with the `s3://` URI.
#'
#' @examples
#' nccs_vintage_url("2023_07")
#' nccs_vintage_url("2023_09", kind = "dictionary")
#' nccs_vintage_url("1999_12", legacy = TRUE)
#'
#' \dontrun{
#' # Read a vintage snapshot directly. Schemas differ across vintages
#' # and across the legacy/modern seam — inspect the matching data
#' # dictionary before filtering or joining.
#' uri <- nccs_vintage_url("2023_07")
#' bmf_2023_07 <- arrow::read_csv_arrow(uri)
#' }
#'
#' @export
nccs_vintage_url <- function(vintage,
                             kind = c("data", "dictionary"),
                             legacy = FALSE) {
  if (!is.character(vintage) || length(vintage) != 1L ||
      !grepl("^\\d{4}_\\d{2}$", vintage)) {
    stop("`vintage` must be a single string of the form 'YYYY_MM'.",
         call. = FALSE)
  }
  kind <- match.arg(kind)
  if (!is.logical(legacy) || length(legacy) != 1L || is.na(legacy)) {
    stop("`legacy` must be TRUE or FALSE.", call. = FALSE)
  }

  prefix <- if (legacy) "processed/bmf-legacy" else "processed/bmf"
  stem   <- if (legacy) "bmf_legacy" else "bmf"
  suffix <- if (kind == "data") "processed" else "data_dictionary"

  sprintf("s3://nccsdata/%s/%s/%s_%s_%s.csv",
          prefix, vintage, stem, vintage, suffix)
}
