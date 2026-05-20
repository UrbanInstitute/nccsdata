#' Convert Nominal Dollars to Real (Inflation-Adjusted) Dollars
#'
#' Adjusts nominal dollar amounts to real dollars expressed in a base
#' year, using the bundled annual CPI-U series ([cpi_u]). The
#' conversion is the standard `nominal * (CPI_base / CPI_year)`.
#'
#' For CORE Form 990 panels covering many years, deflating financial
#' columns is usually a prerequisite for any cross-year comparison.
#' This helper is a thin wrapper around vector arithmetic — it exists
#' to (a) bundle the CPI lookup table so users don't have to fetch it
#' themselves, and (b) standardize the lookup behavior (warning on
#' out-of-series years, NA propagation).
#'
#' @param amount Numeric vector of nominal dollar amounts.
#' @param year Integer vector of years the amounts are denominated in.
#'   Length 1 or `length(amount)`.
#' @param base_year Single integer. The year whose dollars the result
#'   should be expressed in. Must be present in the bundled series.
#' @param cpi Optional override. A two-column data frame with `year`
#'   (integer) and `cpi` (numeric); defaults to the bundled [cpi_u]
#'   table. Useful if you want to use a different deflator (e.g.
#'   PCE, GDP) or a custom snapshot of CPI-U.
#'
#' @return Numeric vector the same length as `amount`, in `base_year`
#'   dollars. `NA` for any input element whose year is not in the
#'   series; a single warning lists the missing years.
#'
#' @seealso [cpi_u] for the bundled series.
#'
#' @examples
#' # $100 in 1990 expressed in 2020 dollars
#' nccs_deflate(100, year = 1990, base_year = 2020)
#'
#' # Vectorized over a column
#' df <- data.frame(year = c(2010, 2015, 2020),
#'                  revenue = c(1e5, 1.2e5, 1.4e5))
#' df$revenue_real_2023 <- nccs_deflate(df$revenue, df$year, base_year = 2023)
#' df
#'
#' @export
nccs_deflate <- function(amount, year, base_year, cpi = NULL) {
  if (!is.numeric(amount)) {
    stop("`amount` must be a numeric vector.", call. = FALSE)
  }
  if (!is.numeric(year)) {
    stop("`year` must be a numeric vector.", call. = FALSE)
  }
  if (length(year) != 1L && length(year) != length(amount)) {
    stop("`year` must be length 1 or length(amount).", call. = FALSE)
  }
  if (!is.numeric(base_year) || length(base_year) != 1L ||
      is.na(base_year) || base_year != as.integer(base_year)) {
    stop("`base_year` must be a single integer year.", call. = FALSE)
  }

  series <- if (is.null(cpi)) nccsdata::cpi_u else cpi
  if (!is.data.frame(series) ||
      !all(c("year", "cpi") %in% names(series))) {
    stop("`cpi` must be a data frame with `year` and `cpi` columns.",
         call. = FALSE)
  }

  base_idx <- match(as.integer(base_year), series$year)
  if (is.na(base_idx)) {
    stop(sprintf(
      "CPI not available for base_year %d. Series covers %d-%d.",
      as.integer(base_year), min(series$year), max(series$year)
    ), call. = FALSE)
  }
  base_cpi <- series$cpi[base_idx]

  year_int <- as.integer(year)
  if (length(year_int) == 1L) year_int <- rep(year_int, length(amount))
  year_cpi <- series$cpi[match(year_int, series$year)]

  missing_years <- unique(year_int[is.na(year_cpi) & !is.na(year_int)])
  if (length(missing_years) > 0L) {
    warning("CPI not available for year(s): ",
            paste(sort(missing_years), collapse = ", "),
            ". Returning NA for those rows.", call. = FALSE)
  }

  amount * (base_cpi / year_cpi)
}
