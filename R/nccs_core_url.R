#' URL of an NCCS CORE Series Partition
#'
#' Returns the public HTTPS URL of a Form 990 CORE Series partition (or
#' its column dictionary) on S3. CORE Series files are one row per
#' filing, partitioned by `(tax_year, form)`. Three tiers are published:
#'
#' \describe{
#'   \item{`"merged"` (default, canonical)}{Legacy + SOI-current merged
#'     on `(ein, tax_period)` with SOI precedence. One row per
#'     `(ein, tax_period)`. Adds `source_pipeline` and
#'     `has_legacy_augment` columns. Tax years 1987-2024. Forms
#'     `990combined`, `990pf`. **Deduplicated** — keeps first occurrence
#'     per `(ein, tax_period)`; use `"soi"` or `"legacy"` instead if
#'     you need every original filing plus amendment.}
#'   \item{`"soi"`}{IRS SOI annual extracts, harmonized. Tax years
#'     2012-2024. Forms `990`, `990ez`, `990pf`, `990combined`. Includes
#'     `is_amendment` for filtering originals vs revisions. 2024 is a
#'     partial year. `990pf` 2017-2019 are present but contain only
#'     backfilled tax-year rows from the 2020+ calendar-year extracts
#'     (no original calendar-year 2017-2019 PF SOI was published); row
#'     counts are small (~665, 3.3k, 100k). For provenance-aware
#'     reads, use the `"merged"` tier (which carries `source_pipeline`
#'     and `has_legacy_augment`).}
#'   \item{`"legacy"`}{NCCS legacy CORE files, harmonized. Tax years
#'     1987-2011. Forms `990combined`, `990pf`. No amendment flag —
#'     duplicates by `(ein, tax_period)` exist but are not
#'     distinguished. `1993/990pf` has only ~11k rows.}
#' }
#'
#' Each partition contains a data file and a column dictionary, each
#' published as both parquet and CSV. Prefer parquet — it supports
#' predicate pushdown and column projection (see [nccs_read_core()]).
#'
#' @param tier One of `"merged"` (default, canonical), `"soi"`, or
#'   `"legacy"`. See Description.
#' @param tax_year Integer tax year.
#' @param form Character form code: `"990"`, `"990ez"`, `"990pf"`, or
#'   `"990combined"`. Not every form exists in every tier — see
#'   Description.
#' @param format One of `"parquet"` (default) or `"csv"`.
#' @param kind One of `"data"` (default) for the filings table or
#'   `"dictionary"` for the per-partition column dictionary.
#'
#' @return A single character HTTPS URL.
#'
#' @seealso [nccs_read_core()] for reading the parquet directly,
#'   [nccs_core_columns()] for the column dictionary,
#'   [nccs_core_coverage()] for a tier's row counts.
#'
#' @examples
#' nccs_core_url("merged", 2020, "990combined")
#' nccs_core_url("soi", 2020, "990", kind = "dictionary")
#' nccs_core_url("legacy", 1995, "990combined", format = "csv")
#'
#' @export
nccs_core_url <- function(tier = c("merged", "soi", "legacy"),
                          tax_year,
                          form,
                          format = c("parquet", "csv"),
                          kind = c("data", "dictionary")) {
  tier   <- match.arg(tier)
  format <- match.arg(format)
  kind   <- match.arg(kind)

  .validate_core_partition(tax_year, form, tier)

  prefix <- switch(tier,
    soi    = "processed/core",
    legacy = "processed_legacy/core",
    merged = "processed_merged/core"
  )
  suffix <- if (kind == "dictionary") "_dictionary" else ""

  sprintf(
    "https://nccsdata.s3.amazonaws.com/%s/%d/%s/core_%d_%s%s.%s",
    prefix, tax_year, form, tax_year, form, suffix, format
  )
}

#' @noRd
.core_valid_forms <- function(tier) {
  switch(tier,
    soi    = c("990", "990ez", "990pf", "990combined"),
    legacy = c("990combined", "990pf"),
    merged = c("990combined", "990pf")
  )
}

#' @noRd
.core_year_range <- function(tier) {
  switch(tier,
    soi    = c(2012L, 2024L),
    legacy = c(1987L, 2011L),
    merged = c(1987L, 2024L)
  )
}

#' Validate (tax_year, form) against a tier's published coverage.
#' @noRd
.validate_core_partition <- function(tax_year, form, tier) {
  if (!is.numeric(tax_year) || length(tax_year) != 1L ||
      is.na(tax_year) || tax_year != as.integer(tax_year)) {
    stop("`tax_year` must be a single integer.", call. = FALSE)
  }
  if (!is.character(form) || length(form) != 1L || is.na(form)) {
    stop("`form` must be a single string.", call. = FALSE)
  }

  valid_forms <- .core_valid_forms(tier)
  if (!form %in% valid_forms) {
    stop(
      "Form '", form, "' is not published in tier '", tier, "'. ",
      "Valid forms: ", paste(valid_forms, collapse = ", "), ".",
      call. = FALSE
    )
  }

  rng <- .core_year_range(tier)
  if (tax_year < rng[1] || tax_year > rng[2]) {
    stop(
      "tax_year ", tax_year, " is outside tier '", tier,
      "' coverage (", rng[1], "-", rng[2], ").",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
