#' Read an NCCS CORE Series Partition
#'
#' Reads one `(tier, tax_year, form)` Form 990 partition as parquet,
#' with optional column projection, dplyr-style filtering, and local
#' caching. One row per filing (or per `(ein, tax_period)` in the
#' deduplicated `"merged"` tier).
#'
#' For multi-year queries, prefer building the dataset yourself with
#' `arrow::open_dataset()` over a glob — see Examples. This function
#' deliberately reads a single partition so caching, column dictionaries,
#' and schema reasoning stay simple.
#'
#' See [nccs_core_url()] for the canonical URL pattern and a description
#' of each tier's coverage and caveats (especially the `"merged"` tier's
#' deduplication and the missing 2017-2019 `990pf` partitions).
#'
#' @inheritParams nccs_core_url
#' @param columns Optional character vector of column names to project.
#'   `NULL` (default) returns all columns. Parquet projection means
#'   unselected columns are never read from disk or wire. Use
#'   [nccs_core_columns()] to see what is available for a partition.
#' @param cache Local cache controls. `TRUE` (default) caches the
#'   parquet under [nccs_cache_dir()] in a `core/<tier>/<tax_year>/<form>/` subdir.
#'   A character path uses that directory instead. `FALSE` reads
#'   directly from S3 (slower on repeat calls, lower disk usage).
#' @param cache_max_age Integer. Maximum age in days before the cached
#'   parquet is considered stale and re-downloaded. Defaults to 30.
#'   Ignored when `cache = FALSE`.
#' @param collect Logical. `TRUE` (default) returns a tibble. `FALSE`
#'   returns a lazy Arrow query for further dplyr operations and a
#'   final `dplyr::collect()`.
#'
#' @return A tibble (if `collect = TRUE`) or an Arrow Dataset query
#'   (if `collect = FALSE`).
#'
#' @seealso [nccs_core_url()], [nccs_core_columns()],
#'   [nccs_core_coverage()].
#'
#' @examples
#' \dontrun{
#' # One partition, a handful of columns
#' df <- nccs_read_core(
#'   tier = "merged",
#'   tax_year = 2020,
#'   form = "990combined",
#'   columns = c("ein", "tax_period", "total_revenue", "total_expenses")
#' )
#'
#' # Lazy query, custom filter, then collect
#' nccs_read_core("merged", 2020, "990combined", collect = FALSE) |>
#'   dplyr::filter(subsection_cd == 3) |>
#'   dplyr::select(ein, tax_period, total_revenue) |>
#'   dplyr::collect()
#'
#' # Multi-year: build the dataset directly
#' arrow::open_dataset(
#'   paste0("s3://nccsdata/processed_merged/core/*/990combined/",
#'          "core_*_990combined.parquet"),
#'   format = "parquet"
#' ) |>
#'   dplyr::filter(tax_year >= 2015) |>
#'   dplyr::collect()
#' }
#'
#' @export
nccs_read_core <- function(tier = c("merged", "soi", "legacy"),
                           tax_year,
                           form,
                           columns = NULL,
                           cache = TRUE,
                           cache_max_age = 30L,
                           collect = TRUE) {
  tier <- match.arg(tier)
  .validate_core_partition(tax_year, form, tier)

  if (!is.null(columns) &&
      (!is.character(columns) || any(is.na(columns)))) {
    stop("`columns` must be a character vector or NULL.", call. = FALSE)
  }

  cache_dir <- .resolve_cache_arg(cache)
  src <- .core_partition_source(
    tier = tier, tax_year = as.integer(tax_year), form = form,
    cache_dir = cache_dir, max_age_days = cache_max_age
  )

  ds <- arrow::open_dataset(src, format = "parquet")
  if (!is.null(columns)) {
    ds <- dplyr::select(ds, dplyr::all_of(columns))
  }

  if (collect) dplyr::as_tibble(dplyr::collect(ds)) else ds
}

#' Decide whether to read a CORE partition from S3 directly or from a
#' local cached copy, downloading on first use or when stale.
#' @noRd
.core_partition_source <- function(tier, tax_year, form,
                                   cache_dir, max_age_days) {
  s3_uri <- .core_partition_s3_uri(tier, tax_year, form)
  if (is.null(cache_dir)) return(s3_uri)

  if (!is.numeric(max_age_days) || length(max_age_days) != 1L ||
      is.na(max_age_days) || max_age_days < 0) {
    stop("`cache_max_age` must be a non-negative number.", call. = FALSE)
  }

  part_dir <- file.path(cache_dir, "core", tier,
                        as.character(tax_year), form)
  local <- file.path(part_dir,
                     sprintf("core_%d_%s.parquet", tax_year, form))
  if (.cache_is_fresh(local, max_age_days)) return(local)

  ok <- tryCatch({
    dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(local, ".part")
    suppressWarnings(
      utils::download.file(
        nccs_core_url(tier, tax_year, form,
                      format = "parquet", kind = "data"),
        tmp, mode = "wb", quiet = TRUE
      )
    )
    file.rename(tmp, local)
    TRUE
  }, error = function(e) {
    warning("nccsdata core cache download failed (",
            conditionMessage(e),
            "); falling back to S3.", call. = FALSE)
    FALSE
  })

  if (ok) local else s3_uri
}

#' @noRd
.core_partition_s3_uri <- function(tier, tax_year, form) {
  prefix <- switch(tier,
    soi    = "processed/core",
    legacy = "processed_legacy/core",
    merged = "processed_merged/core"
  )
  sprintf("s3://nccsdata/%s/%d/%s/core_%d_%s.parquet",
          prefix, tax_year, form, tax_year, form)
}
