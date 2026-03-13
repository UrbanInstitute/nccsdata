#' Read NCCS BMF Data from S3
#'
#' Reads the NCCS Business Master File (BMF) stored as a parquet file in a
#' public S3 bucket. Supports predicate-pushdown filtering by state, county,
#' NTEE subsector, and exempt organization type for efficient reads.
#'
#' @param state Character vector of two-letter state abbreviations to filter on
#'   (e.g., `"PA"`, `c("PA", "NY")`). Filters the `org_addr_state` column.
#' @param ntee_subsector Character vector of NTEE v2 subsector codes (e.g.,
#'   `"ART"`, `c("ART", "EDU")`). See [nccs_catalog()] for valid values.
#'   Filters the `nteev2_subsector` column.
#' @param exempt_org_type Character vector of exempt organization type
#'   descriptions. See [nccs_catalog()] for valid values. Filters the
#'   `exempt_organization_type` column.
#' @param county Character vector of county names (e.g., `"Lackawanna County"`).
#'   Filters the `geo_county` column.
#' @param columns Column selection. `NULL` (default) returns a sensible default
#'   subset. A character vector returns those specific columns. `"all"` returns
#'   all columns (warning: 400+ MB). Columns used in active filters are always
#'   included.
#' @param date Character string in `YYYY_MM` format indicating the data
#'   vintage. Defaults to `"2026_03"`.
#' @param collect Logical. If `TRUE` (default), collects the result into a
#'   tibble. If `FALSE`, returns a lazy Arrow query for further dplyr
#'   operations.
#'
#' @return A tibble (if `collect = TRUE`) or an Arrow Dataset query (if
#'   `collect = FALSE`).
#'
#' @examples
#' \dontrun{
#' # Read all Pennsylvania nonprofits
#' pa <- nccs_read(state = "PA")
#'
#' # Read with specific columns
#' pa <- nccs_read(state = "PA", columns = c("ein", "org_name_display", "geo_county"))
#'
#' # Lazy query for custom dplyr chains
#' query <- nccs_read(state = "PA", collect = FALSE)
#' result <- query |> dplyr::filter(geo_county == "Lackawanna County") |> dplyr::collect()
#' }
#'
#' @importFrom dplyr .data
#' @export
nccs_read <- function(state = NULL,
                      ntee_subsector = NULL,
                      exempt_org_type = NULL,
                      county = NULL,
                      columns = NULL,
                      date = "2026_03",
                      collect = TRUE) {

  # Validate date format
  if (!grepl("^\\d{4}_\\d{2}$", date)) {
    stop("`date` must be in YYYY_MM format (e.g., '2026_03').", call. = FALSE)
  }

  # Validate state codes
  valid_states <- nccs_catalog("state")
  if (!is.null(state)) {
    bad <- setdiff(state, valid_states)
    if (length(bad) > 0) {
      stop(
        "Invalid state code(s): ", paste(bad, collapse = ", "),
        ". Use nccs_catalog('state') to see valid values.",
        call. = FALSE
      )
    }
  }

  # Validate NTEE subsector codes
  valid_ntee <- nccs_catalog("ntee_subsector")
  if (!is.null(ntee_subsector)) {
    bad <- setdiff(ntee_subsector, valid_ntee)
    if (length(bad) > 0) {
      stop(
        "Invalid ntee_subsector code(s): ", paste(bad, collapse = ", "),
        ". Use nccs_catalog('ntee_subsector') to see valid values.",
        call. = FALSE
      )
    }
  }

  # Validate exempt_org_type
  valid_exempt <- nccs_catalog("exempt_org_type")
  if (!is.null(exempt_org_type)) {
    bad <- setdiff(exempt_org_type, valid_exempt)
    if (length(bad) > 0) {
      stop(
        "Invalid exempt_org_type value(s): ", paste(bad, collapse = ", "),
        ". Use nccs_catalog('exempt_org_type') to see valid values.",
        call. = FALSE
      )
    }
  }

  # Open dataset from S3
  s3_path <- .build_s3_path(date)
  ds <- arrow::open_dataset(s3_path)

  # Column selection
  default_columns <- c(
    "ein", "org_name_display", "org_name_raw",
    "org_addr_street", "org_addr_city", "org_addr_state", "org_addr_zip5",
    "geo_county", "geo_city", "geo_state_abbr", "geo_lat", "geo_lon",
    "nteev2", "nteev2_subsector", "nteev2_code", "nteev2_org_type",
    "ntee_code_clean", "ntee_code_definition", "ntee_code_major_group",
    "exempt_organization_type", "subsection_code",
    "ruling_date",
    "asset_amount", "income_amount", "revenue_amount",
    "org_parent_name", "group_exemption_is_member"
  )

  if (is.null(columns)) {
    select_cols <- default_columns
  } else if (length(columns) == 1 && columns == "all") {
    message(
      "Selecting all columns. The parquet file is 400+ MB with 97 columns. ",
      "Consider specifying `columns` for faster reads."
    )
    select_cols <- NULL
  } else {
    select_cols <- columns
  }

  # Ensure filter columns are included in selection
  if (!is.null(select_cols)) {
    filter_cols <- character(0)
    if (!is.null(state)) filter_cols <- c(filter_cols, "org_addr_state")
    if (!is.null(ntee_subsector)) filter_cols <- c(filter_cols, "nteev2_subsector")
    if (!is.null(exempt_org_type)) filter_cols <- c(filter_cols, "exempt_organization_type")
    if (!is.null(county)) filter_cols <- c(filter_cols, "geo_county")
    select_cols <- unique(c(select_cols, filter_cols))
  }

  # Apply column selection (before filters for efficient parquet reads)
  if (!is.null(select_cols)) {
    ds <- dplyr::select(ds, dplyr::all_of(select_cols))
  }

  # Apply row filters at Arrow level (predicate pushdown)
  if (!is.null(state)) {
    ds <- dplyr::filter(ds, .data$org_addr_state %in% state)
  }
  if (!is.null(ntee_subsector)) {
    ds <- dplyr::filter(ds, .data$nteev2_subsector %in% ntee_subsector)
  }
  if (!is.null(exempt_org_type)) {
    ds <- dplyr::filter(ds, .data$exempt_organization_type %in% exempt_org_type)
  }
  if (!is.null(county)) {
    ds <- dplyr::filter(ds, .data$geo_county %in% county)
  }

  if (collect) {
    dplyr::as_tibble(dplyr::collect(ds))
  } else {
    ds
  }
}

#' Build S3 path to BMF parquet file
#' @param date Character string in YYYY_MM format
#' @return Character string with S3 URI
#' @noRd
.build_s3_path <- function(date) {
  paste0(
    "s3://nccsdata/geocoding/bmf/", date,
    "/merged/bmf_", date, "_geocoded.parquet"
  )
}
