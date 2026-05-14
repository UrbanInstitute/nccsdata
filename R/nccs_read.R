#' Read NCCS BMF Data from S3
#'
#' Reads the NCCS Business Master File (BMF) stored as a parquet file in a
#' public S3 bucket. Supports predicate-pushdown filtering on state, county,
#' NTEE classification (subsector, code, NTEEv2 code, major group), exempt
#' organization type, financial size, and BMF recency for efficient reads.
#'
#' Reads the rolling "master" geocoded BMF at
#' `s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet`.
#' For a specific dated monthly snapshot, see [nccs_vintage_url()] — those
#' artifacts are CSVs with per-vintage schemas and are not exposed through
#' this function.
#'
#' @param state Character vector of two-letter state abbreviations (e.g.,
#'   `"PA"`, `c("PA", "NY")`). Filters `org_addr_state`.
#' @param county Character vector of county names (e.g.,
#'   `"Lackawanna County"`). Filters `geo_county`.
#' @param ntee_subsector Character vector of NTEEv2 subsector values.
#'   Accepts either subsector codes (`"UNI"`, `"ART"`) or human-readable
#'   names (`"Universities"`, `"Arts, Culture and Humanities"`), matched
#'   case-insensitively. See [nccs_catalog()] for valid values. Filters
#'   `nteev2_subsector`.
#' @param ntee_major_group Character vector of single-letter NTEE major
#'   groups (`"A"` through `"Z"`). Filters `ntee_code_major_group`. See
#'   [nccs_catalog("ntee_major_group", labels = TRUE)] for descriptions.
#' @param ntee_code Character vector of standardized 3-character NTEE
#'   codes (e.g., `"B40"`, `c("A20", "A23")`). Filters `ntee_code_clean`.
#'   Not validated — invalid codes just return no rows.
#' @param nteev2_code Character vector of NTEEv2 3-character codes.
#'   Filters `nteev2_code`. Not validated.
#' @param exempt_org_type Character vector of exempt organization type
#'   descriptions. See [nccs_catalog()] for valid values. Filters
#'   `exempt_organization_type`.
#' @param org_type Convenience filter that combines `subsection_code`
#'   and `foundation_code` into the four common 501(c)(3) cuts plus
#'   their complement. One of:
#'   \itemize{
#'     \item `"all"` (default) — no filter.
#'     \item `"501c3"` — every 501(c)(3) organization
#'       (`subsection_code == "3"`).
#'     \item `"public_charity"` / `"pc"` — 501(c)(3) public charities
#'       (foundation codes other than the three private-foundation
#'       types).
#'     \item `"private_foundation"` / `"pf"` — 501(c)(3) private
#'       foundations (`foundation_code` in `"2"`, `"3"`, `"4"`).
#'     \item `"non_501c3"` — everything that is not a 501(c)(3).
#'   }
#' @param size_metric One of `"revenue"`, `"income"`, or `"asset"`
#'   indicating which financial amount to use with `size_min` / `size_max`.
#'   Defaults to `"revenue"`. The underlying columns are stored upstream
#'   as character; this function casts to numeric inside the predicate so
#'   the filter pushes down to Arrow.
#' @param size_min,size_max Numeric. Optional inclusive bounds on the
#'   `size_metric` amount. `NULL` (default) leaves a side unbounded. Rows
#'   with `NA` for the chosen metric are dropped when either bound is set.
#' @param min_last_year Integer. If set, restricts results to EINs whose
#'   `last_year_in_bmf` (the calendar year of the most recent BMF vintage
#'   in which the EIN appeared) is at least this value. Use this as a
#'   recency / "still active" filter — e.g., `min_last_year = 2024` keeps
#'   organizations seen in BMF in 2024 or later.
#' @param coerce Logical. If `TRUE` (default), known character-typed
#'   financial, date, and indicator columns are coerced to their natural
#'   R types after collection (see "Column coercion" below). Set to
#'   `FALSE` to leave every column as published upstream. Only takes
#'   effect when `collect = TRUE`; with `collect = FALSE` the lazy
#'   Arrow query is returned untouched.
#' @param cache Controls local caching of the master parquet. The S3
#'   file is hundreds of MB; without caching each call re-downloads it.
#'   `TRUE` (default) caches in `tools::R_user_dir("nccsdata", "cache")`.
#'   A character path uses that directory instead. `FALSE` skips the
#'   cache and reads directly from S3 every call. See
#'   [nccs_cache_dir()] and [nccs_cache_clear()].
#' @param cache_max_age Integer. Maximum age in days before the cached
#'   parquet is considered stale and re-downloaded. Defaults to 30 (the
#'   master is rebuilt monthly upstream). Ignored when `cache = FALSE`.
#' @param columns Column selection. `NULL` (default) returns a sensible
#'   default subset. A character vector returns those specific columns.
#'   `"all"` returns all columns (warning: 400+ MB). Columns used in
#'   active filters are always included.
#' @param collect Logical. If `TRUE` (default), collects the result into
#'   a tibble. If `FALSE`, returns a lazy Arrow query for further dplyr
#'   operations.
#'
#' @return A tibble (if `collect = TRUE`) or an Arrow Dataset query (if
#'   `collect = FALSE`).
#'
#' @section Column coercion:
#' The upstream BMF parquet stores most columns as `character` by design
#' (vintage-stacking requires it). When `coerce = TRUE`, `nccs_read()`
#' casts the following on the collected tibble:
#' \itemize{
#'   \item Numeric: `asset_amount`, `income_amount`, `revenue_amount`,
#'     `geo_score`, `geo_distance`.
#'   \item Date (`YYYY-MM-DD`): `ruling_date`, `tax_period_ymd`.
#'   \item Logical indicators (via [nccs_as_indicator()] with the `yn`
#'     scheme): `group_exemption_is_member`, `org_addr_is_po_box`,
#'     `org_addr_is_rural_route`, `org_addr_has_special_chars`,
#'     `org_addr_is_missing`, `org_addr_missing_number`,
#'     `org_addr_state_invalid`, `ruling_date_is_missing`,
#'     `tax_period_is_missing`, `in_care_of_name_provided`.
#' }
#' Code-like columns (e.g. `subsection_code`, `classification_code`,
#' ZIPs) are intentionally left as `character` — they are identifiers,
#' not numbers. Columns not in the result (because of `columns`
#' selection) are silently skipped.
#'
#' @examples
#' \dontrun{
#' # All Pennsylvania nonprofits
#' pa <- nccs_read(state = "PA")
#'
#' # Universities in PA seen in BMF in 2024 or later
#' pa_uni <- nccs_read(
#'   state = "PA",
#'   ntee_subsector = "Universities",
#'   min_last_year = 2024
#' )
#'
#' # Arts orgs (major group A) with revenue between $1M and $10M
#' arts_mid <- nccs_read(
#'   ntee_major_group = "A",
#'   size_metric = "revenue",
#'   size_min = 1e6,
#'   size_max = 1e7
#' )
#'
#' # Only 501(c)(3) private foundations in California
#' ca_pf <- nccs_read(state = "CA", org_type = "private_foundation")
#'
#' # Lazy query for custom dplyr chains
#' query <- nccs_read(state = "PA", collect = FALSE)
#' result <- query |>
#'   dplyr::filter(geo_county == "Lackawanna County") |>
#'   dplyr::collect()
#' }
#'
#' @importFrom dplyr .data
#' @export
nccs_read <- function(state = NULL,
                      county = NULL,
                      ntee_subsector = NULL,
                      ntee_major_group = NULL,
                      ntee_code = NULL,
                      nteev2_code = NULL,
                      exempt_org_type = NULL,
                      org_type = c("all", "501c3", "public_charity", "pc",
                                   "private_foundation", "pf", "non_501c3"),
                      size_metric = c("revenue", "income", "asset"),
                      size_min = NULL,
                      size_max = NULL,
                      min_last_year = NULL,
                      cache = TRUE,
                      cache_max_age = 30L,
                      coerce = TRUE,
                      columns = NULL,
                      collect = TRUE) {

  size_metric <- match.arg(size_metric)
  org_type <- match.arg(org_type)
  cache_dir <- .resolve_cache_arg(cache)
  if (!is.logical(coerce) || length(coerce) != 1L || is.na(coerce)) {
    stop("`coerce` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.null(state)) {
    valid_states <- nccs_catalog("state")
    bad <- setdiff(state, valid_states)
    if (length(bad) > 0) {
      stop(
        "Invalid state code(s): ", paste(bad, collapse = ", "),
        ". Use nccs_catalog('state') to see valid values.",
        call. = FALSE
      )
    }
  }

  if (!is.null(ntee_subsector)) {
    ntee_subsector <- .resolve_ntee_subsector(ntee_subsector)
  }

  if (!is.null(ntee_major_group)) {
    bad <- setdiff(ntee_major_group, LETTERS)
    if (length(bad) > 0) {
      stop(
        "Invalid ntee_major_group value(s): ", paste(bad, collapse = ", "),
        ". Must be single uppercase letters A-Z.",
        call. = FALSE
      )
    }
  }

  if (!is.null(exempt_org_type)) {
    valid_exempt <- nccs_catalog("exempt_org_type")
    bad <- setdiff(exempt_org_type, valid_exempt)
    if (length(bad) > 0) {
      stop(
        "Invalid exempt_org_type value(s): ", paste(bad, collapse = ", "),
        ". Use nccs_catalog('exempt_org_type') to see valid values.",
        call. = FALSE
      )
    }
  }

  if (!is.null(size_min) && (!is.numeric(size_min) || length(size_min) != 1L)) {
    stop("`size_min` must be a single numeric value or NULL.", call. = FALSE)
  }
  if (!is.null(size_max) && (!is.numeric(size_max) || length(size_max) != 1L)) {
    stop("`size_max` must be a single numeric value or NULL.", call. = FALSE)
  }
  if (!is.null(size_min) && !is.null(size_max) && size_min > size_max) {
    stop("`size_min` must be <= `size_max`.", call. = FALSE)
  }

  if (!is.null(min_last_year)) {
    if (!is.numeric(min_last_year) || length(min_last_year) != 1L ||
        min_last_year != as.integer(min_last_year)) {
      stop("`min_last_year` must be a single integer value or NULL.", call. = FALSE)
    }
    min_last_year <- as.integer(min_last_year)
  }

  ds <- arrow::open_dataset(
    .bmf_master_source(cache_dir = cache_dir, max_age_days = cache_max_age)
  )

  default_columns <- c(
    "ein", "org_name_display", "org_name_raw",
    "org_addr_street", "org_addr_city", "org_addr_state", "org_addr_zip5",
    "geo_county", "geo_city", "geo_state_abbr", "geo_lat", "geo_lon",
    "nteev2", "nteev2_subsector", "nteev2_code", "nteev2_org_type",
    "ntee_code_clean", "ntee_code_definition", "ntee_code_major_group",
    "exempt_organization_type", "subsection_code", "foundation_code",
    "ruling_date",
    "asset_amount", "income_amount", "revenue_amount",
    "org_parent_name", "group_exemption_is_member",
    "last_year_in_bmf"
  )

  size_col <- switch(size_metric,
    revenue = "revenue_amount",
    income  = "income_amount",
    asset   = "asset_amount"
  )

  if (is.null(columns)) {
    select_cols <- default_columns
  } else if (length(columns) == 1 && columns == "all") {
    message(
      "Selecting all columns. The parquet file is 400+ MB with 100+ columns. ",
      "Consider specifying `columns` for faster reads."
    )
    select_cols <- NULL
  } else {
    select_cols <- columns
  }

  if (!is.null(select_cols)) {
    filter_cols <- character(0)
    if (!is.null(state))            filter_cols <- c(filter_cols, "org_addr_state")
    if (!is.null(county))           filter_cols <- c(filter_cols, "geo_county")
    if (!is.null(ntee_subsector))   filter_cols <- c(filter_cols, "nteev2_subsector")
    if (!is.null(ntee_major_group)) filter_cols <- c(filter_cols, "ntee_code_major_group")
    if (!is.null(ntee_code))        filter_cols <- c(filter_cols, "ntee_code_clean")
    if (!is.null(nteev2_code))      filter_cols <- c(filter_cols, "nteev2_code")
    if (!is.null(exempt_org_type))  filter_cols <- c(filter_cols, "exempt_organization_type")
    if (!is.null(size_min) || !is.null(size_max)) filter_cols <- c(filter_cols, size_col)
    if (!is.null(min_last_year))    filter_cols <- c(filter_cols, "last_year_in_bmf")
    if (org_type != "all")          filter_cols <- c(filter_cols, "subsection_code", "foundation_code")
    select_cols <- unique(c(select_cols, filter_cols))
  }

  if (!is.null(select_cols)) {
    ds <- dplyr::select(ds, dplyr::all_of(select_cols))
  }

  if (!is.null(state)) {
    ds <- dplyr::filter(ds, .data$org_addr_state %in% state)
  }
  if (!is.null(county)) {
    ds <- dplyr::filter(ds, .data$geo_county %in% county)
  }
  if (!is.null(ntee_subsector)) {
    ds <- dplyr::filter(ds, .data$nteev2_subsector %in% ntee_subsector)
  }
  if (!is.null(ntee_major_group)) {
    ds <- dplyr::filter(ds, .data$ntee_code_major_group %in% ntee_major_group)
  }
  if (!is.null(ntee_code)) {
    ds <- dplyr::filter(ds, .data$ntee_code_clean %in% ntee_code)
  }
  if (!is.null(nteev2_code)) {
    ds <- dplyr::filter(ds, .data$nteev2_code %in% nteev2_code)
  }
  if (!is.null(exempt_org_type)) {
    ds <- dplyr::filter(ds, .data$exempt_organization_type %in% exempt_org_type)
  }
  if (!is.null(size_min)) {
    ds <- dplyr::filter(ds, as.numeric(.data[[size_col]]) >= size_min)
  }
  if (!is.null(size_max)) {
    ds <- dplyr::filter(ds, as.numeric(.data[[size_col]]) <= size_max)
  }
  if (!is.null(min_last_year)) {
    ds <- dplyr::filter(ds, .data$last_year_in_bmf >= min_last_year)
  }
  pf_codes <- c("2", "3", "4")
  if (org_type %in% c("501c3")) {
    ds <- dplyr::filter(ds, .data$subsection_code == "3")
  } else if (org_type %in% c("public_charity", "pc")) {
    ds <- dplyr::filter(
      ds,
      .data$subsection_code == "3" & !.data$foundation_code %in% pf_codes
    )
  } else if (org_type %in% c("private_foundation", "pf")) {
    ds <- dplyr::filter(
      ds,
      .data$subsection_code == "3" & .data$foundation_code %in% pf_codes
    )
  } else if (org_type == "non_501c3") {
    ds <- dplyr::filter(ds, .data$subsection_code != "3")
  }

  if (collect) {
    out <- dplyr::as_tibble(dplyr::collect(ds))
    if (coerce) out <- .coerce_bmf_columns(out)
    out
  } else {
    ds
  }
}

#' Apply documented natural-type coercions to a collected BMF tibble.
#' Columns absent from `df` are skipped silently.
#' @noRd
.coerce_bmf_columns <- function(df) {
  spec <- .bmf_coerce_spec()
  for (col in intersect(spec$numeric, names(df))) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  for (col in intersect(spec$date, names(df))) {
    df[[col]] <- suppressWarnings(as.Date(df[[col]]))
  }
  for (col in intersect(spec$indicator, names(df))) {
    df[[col]] <- suppressWarnings(nccs_as_indicator(df[[col]], scheme = "yn"))
  }
  df
}

#' @noRd
.bmf_coerce_spec <- function() {
  list(
    numeric = c(
      "asset_amount", "income_amount", "revenue_amount",
      "geo_score", "geo_distance"
    ),
    date = c("ruling_date", "tax_period_ymd"),
    indicator = c(
      "group_exemption_is_member",
      "org_addr_is_po_box", "org_addr_is_rural_route",
      "org_addr_has_special_chars", "org_addr_is_missing",
      "org_addr_missing_number", "org_addr_state_invalid",
      "ruling_date_is_missing", "tax_period_is_missing",
      "in_care_of_name_provided"
    )
  )
}

#' Resolve a user-supplied ntee_subsector vector (codes or names) to codes
#' @noRd
.resolve_ntee_subsector <- function(x) {
  tbl <- .nccs_lookups$nteev2_subsector
  valid_codes <- as.character(tbl$nteev2_subsector)
  name_to_code <- valid_codes
  names(name_to_code) <- tolower(as.character(tbl$nteev2_subsector_definition))

  resolved <- vapply(x, function(v) {
    if (v %in% valid_codes) return(v)
    # Accept inline "CODE - Description" form from nccs_catalog("ntee_subsector")
    head <- sub(" - .*$", "", v)
    if (head %in% valid_codes) return(head)
    hit <- name_to_code[tolower(v)]
    if (!is.na(hit)) return(unname(hit))
    NA_character_
  }, character(1))

  bad <- x[is.na(resolved)]
  if (length(bad) > 0) {
    stop(
      "Invalid ntee_subsector value(s): ", paste(bad, collapse = ", "),
      ". Use nccs_catalog('ntee_subsector') to see valid codes and names.",
      call. = FALSE
    )
  }
  unname(resolved)
}

#' S3 URI of the rolling master geocoded BMF parquet
#' @return Character string with S3 URI
#' @noRd
.bmf_master_s3_path <- function() {
  "s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet"
}

#' Public HTTPS URL mirroring the S3 path; used for local cache downloads.
#' @noRd
.bmf_master_https_url <- function() {
  "https://nccsdata.s3.amazonaws.com/geocoding/bmf-master/merged/bmf_master_geocoded.parquet"
}

#' Resolve the `cache` argument of nccs_read() to a directory path or NULL.
#' @noRd
.resolve_cache_arg <- function(cache) {
  if (isFALSE(cache)) return(NULL)
  if (isTRUE(cache)) return(tools::R_user_dir("nccsdata", which = "cache"))
  if (is.character(cache) && length(cache) == 1L && !is.na(cache)) return(cache)
  stop("`cache` must be TRUE, FALSE, or a single directory path.", call. = FALSE)
}

#' Decide whether to read from S3 directly or from a local cached copy,
#' downloading on first use or when the cached file is stale.
#' @noRd
.bmf_master_source <- function(cache_dir, max_age_days) {
  if (is.null(cache_dir)) return(.bmf_master_s3_path())

  if (!is.numeric(max_age_days) || length(max_age_days) != 1L ||
      is.na(max_age_days) || max_age_days < 0) {
    stop("`cache_max_age` must be a non-negative number.", call. = FALSE)
  }

  local <- file.path(cache_dir, "bmf_master_geocoded.parquet")
  if (.cache_is_fresh(local, max_age_days)) return(local)

  ok <- tryCatch({
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(local, ".part")
    suppressWarnings(
      utils::download.file(.bmf_master_https_url(), tmp,
                           mode = "wb", quiet = TRUE)
    )
    file.rename(tmp, local)
    TRUE
  }, error = function(e) {
    warning("nccsdata cache download failed (", conditionMessage(e),
            "); falling back to S3.", call. = FALSE)
    FALSE
  })

  if (ok) local else .bmf_master_s3_path()
}

#' @noRd
.cache_is_fresh <- function(path, max_age_days) {
  if (!file.exists(path)) return(FALSE)
  age <- as.numeric(difftime(Sys.time(), file.mtime(path), units = "days"))
  age <= max_age_days
}
