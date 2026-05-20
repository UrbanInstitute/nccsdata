#' Read NCCS CORE Series Partitions
#'
#' Reads one or more `(tier, tax_year, form)` Form 990 partitions as
#' parquet, with optional column projection, dplyr-style filtering, and
#' local caching. Pass a vector of years to `tax_year` to read multiple
#' partitions in a single query (e.g. `tax_year = 2015:2022`); the
#' partitions are stacked into one Arrow dataset so filters and column
#' projection push down across all of them.
#'
#' One row per filing (or per `(ein, tax_period)` in the deduplicated
#' `"merged"` tier).
#'
#' When the request would download multiple partitions, the function
#' reports the total transfer size (summed over partitions not already
#' cached) and, if `confirm = TRUE`, prompts before downloading.
#' Partitions that are not published for the requested tier (e.g. SOI
#' `990pf` for 2017-2019) are dropped with a `message()` rather than
#' raising an error, so a year range that straddles a gap still works.
#'
#' See [nccs_core_url()] for the canonical URL pattern and a description
#' of each tier's coverage and caveats (especially the `"merged"` tier's
#' deduplication and the missing 2017-2019 `990pf` partitions).
#'
#' @inheritParams nccs_core_url
#' @param tax_year Integer tax year, or an integer vector of years for a
#'   multi-partition read.
#' @param columns Optional character vector of column names to project.
#'   `NULL` (default) returns all columns. Parquet projection means
#'   unselected columns are never read from disk or wire. Use
#'   [nccs_core_columns()] to see what is available for a partition.
#' @param cache Local cache controls. `TRUE` (default) caches each
#'   parquet under [nccs_cache_dir()] in a
#'   `core/<tier>/<tax_year>/<form>/` subdir. A character path uses that
#'   directory instead. `FALSE` reads directly from S3 (slower on repeat
#'   calls, lower disk usage).
#' @param cache_max_age Integer. Maximum age in days before the cached
#'   parquet is considered stale and re-downloaded. Defaults to 30.
#'   Ignored when `cache = FALSE`.
#' @param collect Logical. `TRUE` (default) returns a tibble. `FALSE`
#'   returns a lazy Arrow query for further dplyr operations and a
#'   final `dplyr::collect()`.
#' @param confirm Logical. If `TRUE` and a multi-partition download is
#'   required, prompt for confirmation after reporting the total size.
#'   Defaults to `interactive()` so scripts and tests proceed silently
#'   while interactive sessions get a guardrail.
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
#' # Multi-year: any partition that turns out to be unpublished
#' # upstream is dropped with a message rather than erroring.
#' panel <- nccs_read_core(
#'   tier     = "soi",
#'   tax_year = 2015:2022,
#'   form     = "990pf",
#'   columns  = c("ein", "tax_period", "total_revenue")
#' )
#'
#' # Lazy query, custom filter, then collect
#' nccs_read_core("merged", 2018:2022, "990combined", collect = FALSE) |>
#'   dplyr::filter(subsection_cd == 3) |>
#'   dplyr::select(ein, tax_period, total_revenue) |>
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
                           collect = TRUE,
                           confirm = interactive()) {
  tier <- match.arg(tier)
  if (!is.numeric(tax_year) || length(tax_year) == 0L ||
      any(is.na(tax_year)) ||
      any(tax_year != as.integer(tax_year))) {
    stop("`tax_year` must be a non-empty integer vector with no NAs.",
         call. = FALSE)
  }
  tax_years <- sort(unique(as.integer(tax_year)))
  for (y in tax_years) .validate_core_partition(y, form, tier)

  if (!is.null(columns) &&
      (!is.character(columns) || any(is.na(columns)))) {
    stop("`columns` must be a character vector or NULL.", call. = FALSE)
  }

  cache_dir <- .resolve_cache_arg(cache)
  srcs <- .core_resolve_sources(
    tier = tier, tax_years = tax_years, form = form,
    cache_dir = cache_dir, max_age_days = cache_max_age,
    confirm = isTRUE(confirm)
  )

  ds <- arrow::open_dataset(srcs, format = "parquet")
  if (!is.null(columns)) {
    ds <- dplyr::select(ds, dplyr::all_of(columns))
  }
  if (collect) dplyr::as_tibble(dplyr::collect(ds)) else ds
}

#' Resolve one or more CORE partitions to readable paths/URIs,
#' downloading missing pieces to the local cache after reporting total
#' transfer size (and, if `confirm`, prompting).
#' @noRd
.core_resolve_sources <- function(tier, tax_years, form,
                                  cache_dir, max_age_days, confirm) {
  if (!is.numeric(max_age_days) || length(max_age_days) != 1L ||
      is.na(max_age_days) || max_age_days < 0) {
    stop("`cache_max_age` must be a non-negative number.", call. = FALSE)
  }

  plan <- lapply(tax_years, function(y) {
    s3   <- .core_partition_s3_uri(tier, y, form)
    have_cache <- !is.null(cache_dir)
    local <- if (have_cache) {
      file.path(cache_dir, "core", tier, as.character(y), form,
                sprintf("core_%d_%s.parquet", y, form))
    } else NA_character_

    if (have_cache && .cache_is_fresh(local, max_age_days)) {
      return(list(year = y, s3 = s3, path = local,
                  exists = TRUE, cached = TRUE, size = NA_real_))
    }
    info <- .core_partition_remote_info(tier, y, form)
    list(year   = y,
         s3     = s3,
         path   = if (have_cache) local else s3,
         exists = info$exists,
         cached = FALSE,
         size   = info$size)
  })

  exists_flags <- vapply(plan, `[[`, logical(1), "exists")
  if (!all(exists_flags)) {
    missing_years <- vapply(plan[!exists_flags], `[[`, integer(1), "year")
    message("Skipping ", length(missing_years),
            " partition(s) not published for tier='", tier,
            "', form='", form, "': ",
            paste(missing_years, collapse = ", "), ".")
    plan <- plan[exists_flags]
    if (length(plan) == 0L) {
      stop("No published CORE partitions for the requested tier/form/years.",
           call. = FALSE)
    }
  }

  if (!is.null(cache_dir)) {
    cached_flags <- vapply(plan, `[[`, logical(1), "cached")
    to_dl <- plan[!cached_flags]
    if (length(to_dl) > 0) {
      total <- sum(vapply(to_dl, `[[`, numeric(1), "size"), na.rm = TRUE)
      yrs   <- vapply(to_dl, `[[`, integer(1), "year")
      message(sprintf(
        "nccs_read_core: downloading %d partition(s) (%s) to local cache for years %s.",
        length(to_dl), .format_bytes(total), paste(yrs, collapse = ", ")
      ))
      if (confirm) {
        ans <- utils::askYesNo("Continue?", default = TRUE)
        if (!isTRUE(ans)) {
          stop("Download aborted by user.", call. = FALSE)
        }
      }
      for (p in to_dl) {
        .download_core_partition(tier, p$year, form, p$path)
      }
    }
  }

  vapply(plan, `[[`, character(1), "path")
}

#' HEAD-style probe of a CORE partition on S3. Returns
#' `list(exists, size)` with `size` in bytes when known.
#' @noRd
.core_partition_remote_info <- function(tier, tax_year, form) {
  key <- sub("^s3://", "", .core_partition_s3_uri(tier, tax_year, form))
  tryCatch({
    fs <- arrow::S3FileSystem$create(anonymous = TRUE)
    info <- fs$GetFileInfo(key)
    # GetFileInfo on a single key returns a length-1 list of FileInfo;
    # multi-key calls return a longer list. Always unwrap.
    if (is.list(info)) info <- info[[1L]]
    # arrow FileType enum: 0 = NotFound, 2 = File. Anything that isn't a
    # File should be treated as missing for our purposes.
    tp <- tryCatch(as.integer(info$type), error = function(e) NA_integer_)
    if (is.na(tp) || tp != 2L) {
      return(list(exists = FALSE, size = NA_real_))
    }
    sz <- tryCatch(as.numeric(info$size), error = function(e) NA_real_)
    if (is.null(sz) || length(sz) != 1L || is.na(sz) || sz <= 0) {
      list(exists = FALSE, size = NA_real_)
    } else {
      list(exists = TRUE, size = sz)
    }
  }, error = function(e) list(exists = FALSE, size = NA_real_))
}

#' Download a single CORE partition into `local_path`. Caller is
#' responsible for having confirmed total transfer size.
#' @noRd
.download_core_partition <- function(tier, tax_year, form, local_path) {
  part_dir <- dirname(local_path)
  dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)
  url <- nccs_core_url(tier, tax_year, form,
                       format = "parquet", kind = "data")
  tmp <- paste0(local_path, ".part")
  ok <- tryCatch({
    suppressWarnings(
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
    )
    file.rename(tmp, local_path)
    TRUE
  }, error = function(e) {
    warning("nccsdata core cache download failed for ",
            tax_year, "/", form, " (", conditionMessage(e), ").",
            call. = FALSE)
    FALSE
  })
  invisible(ok)
}

#' Format a non-negative byte count as a human-readable string.
#' @noRd
.format_bytes <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n <= 0) {
    return("size unknown")
  }
  units <- c("B", "KB", "MB", "GB", "TB")
  i <- min(length(units), max(1L, floor(log(n, 1024)) + 1L))
  sprintf("%.1f %s", n / 1024^(i - 1), units[i])
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
