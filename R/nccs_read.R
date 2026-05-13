#' Read NCCS BMF Data from S3
#'
#' Reads the NCCS Business Master File (BMF) stored as a parquet file in a
#' public S3 bucket. Supports predicate-pushdown filtering on state, county,
#' NTEE classification (subsector, code, NTEEv2 code, major group), exempt
#' organization type, financial size, and BMF recency for efficient reads.
#'
#' The package reads the rolling "master" geocoded BMF published at
#' `s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet`.
#' The upstream pipeline (`../nccs-data-bmf/`) also publishes dated monthly
#' snapshots at `s3://nccsdata/geocoding/bmf/{YYYY_MM}/...` but this package
#' does not expose them — point `arrow::open_dataset()` at that path
#' directly if you need a specific vintage.
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
                      size_metric = c("revenue", "income", "asset"),
                      size_min = NULL,
                      size_max = NULL,
                      min_last_year = NULL,
                      columns = NULL,
                      collect = TRUE) {

  size_metric <- match.arg(size_metric)

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

  ds <- arrow::open_dataset(.bmf_master_s3_path())

  default_columns <- c(
    "ein", "org_name_display", "org_name_raw",
    "org_addr_street", "org_addr_city", "org_addr_state", "org_addr_zip5",
    "geo_county", "geo_city", "geo_state_abbr", "geo_lat", "geo_lon",
    "nteev2", "nteev2_subsector", "nteev2_code", "nteev2_org_type",
    "ntee_code_clean", "ntee_code_definition", "ntee_code_major_group",
    "exempt_organization_type", "subsection_code",
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

  if (collect) {
    dplyr::as_tibble(dplyr::collect(ds))
  } else {
    ds
  }
}

#' Resolve a user-supplied ntee_subsector vector (codes or names) to codes
#' @noRd
.resolve_ntee_subsector <- function(x) {
  valid_codes <- nccs_catalog("ntee_subsector")
  tbl <- .nccs_lookups$nteev2_subsector
  name_to_code <- as.character(tbl$nteev2_subsector)
  names(name_to_code) <- tolower(as.character(tbl$nteev2_subsector_definition))

  resolved <- vapply(x, function(v) {
    if (v %in% valid_codes) return(v)
    hit <- name_to_code[tolower(v)]
    if (!is.na(hit)) return(unname(hit))
    NA_character_
  }, character(1))

  bad <- x[is.na(resolved)]
  if (length(bad) > 0) {
    stop(
      "Invalid ntee_subsector value(s): ", paste(bad, collapse = ", "),
      ". Use nccs_catalog('ntee_subsector', labels = TRUE) to see valid ",
      "codes and names.",
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
