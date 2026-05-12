#' Normalize EINs to Canonical IRS Format
#'
#' Coerces a vector of Employer Identification Numbers (EINs) to the canonical
#' IRS display format `XX-XXXXXXX` (10 characters, two digits, hyphen, seven
#' digits). Useful for users joining external data (CSV exports, spreadsheets,
#' API responses) against the BMF returned by [nccs_read()], whose `ein` column
#' is already normalized upstream.
#'
#' The function strips all non-digit characters, left-pads to nine digits with
#' zeros (to recover EINs that lost a leading zero during numeric coercion),
#' and inserts the hyphen at position 3. Values that cannot be reduced to a
#' non-empty digit string of nine or fewer characters become `NA_character_`.
#'
#' Pure base-R, no side effects, no in-place mutation.
#'
#' @param x A character or numeric vector of EINs in any format
#'   (`"123456789"`, `"12-3456789"`, `123456789`, `"1234567"`, etc.).
#'
#' @return A character vector the same length as `x`, with each element either
#'   in `XX-XXXXXXX` form or `NA_character_`.
#'
#' @examples
#' nccs_normalize_ein(c("123456789", "12-3456789", 123456789))
#' # All three become "12-3456789".
#'
#' nccs_normalize_ein("1234567")       # leading zeros restored -> "00-1234567"
#' nccs_normalize_ein(c("abc", NA))    # both become NA
#'
#' @export
nccs_normalize_ein <- function(x) {
  if (length(x) == 0L) return(character(0))

  raw <- as.character(x)
  digits <- gsub("[^0-9]", "", trimws(raw))

  ok <- !is.na(raw) & nchar(digits) >= 1L & nchar(digits) <= 9L
  padded <- ifelse(
    ok,
    formatC(as.numeric(digits), width = 9L, flag = "0", format = "d"),
    NA_character_
  )

  ifelse(
    is.na(padded),
    NA_character_,
    paste0(substr(padded, 1L, 2L), "-", substr(padded, 3L, 9L))
  )
}
