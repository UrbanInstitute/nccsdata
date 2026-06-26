#' Reduce an EIN to its 9-Digit Core
#'
#' Internal implementation of the canonical `core()` normalization in the
#' `nccs-contracts` EIN-format convention (`conventions/ein-format.md` Section 2):
#' strip all non-digit characters, reject inputs with more than nine digits
#' (do not truncate), left-zero-pad to width nine (recovering EINs that lost a
#' leading zero on the bare-integer surface), then reject the all-zeros
#' placeholder. Un-normalizable inputs become `NA_character_`.
#'
#' Pure base-R, vectorized over a character vector. This is the single source of
#' truth that both [nccs_ein_to_ein2()] and [nccs_ein2_to_ein()] route through,
#' kept consistent with the producer's `nccs-data-bmf/R/ein.R::transform_ein`.
#'
#' @param x A character or numeric vector of EIN-like values.
#' @return A character vector of nine-digit cores, with `NA_character_` for
#'   values that fail normalization.
#' @noRd
.ein_core <- function(x) {
  raw <- as.character(x)
  digits <- gsub("[^0-9]", "", trimws(raw))

  # >9 digits is invalid (do not truncate); 0 digits is invalid too.
  ok <- !is.na(raw) & nchar(digits) >= 1L & nchar(digits) <= 9L
  core <- ifelse(
    ok,
    formatC(as.numeric(digits), width = 9L, flag = "0", format = "d"),
    NA_character_
  )

  # Reject the all-zeros placeholder; it is not an EIN.
  core[!is.na(core) & core == "000000000"] <- NA_character_
  core
}

#' Warn When EIN Conversion Dropped Non-Empty Input to NA
#'
#' Makes lenient (NA-returning) loss visible rather than silent, per the
#' convention's validation guidance. Counts only inputs that carried a value
#' (non-`NA`, non-blank) yet produced `NA` output.
#'
#' @param input The original vector passed to the converter.
#' @param output The converted character vector.
#' @return `invisible(NULL)`, called for its warning side effect.
#' @noRd
.warn_ein_dropped <- function(input, output) {
  raw <- trimws(as.character(input))
  had_value <- !is.na(raw) & nzchar(raw)
  dropped <- sum(had_value & is.na(output))
  if (dropped > 0L) {
    warning(
      sprintf(
        "%d value%s could not be normalized to a valid EIN and became NA.",
        dropped, if (dropped == 1L) "" else "s"
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Bridge Between `ein` and `EIN2` Formats
#'
#' Convert between the canonical clean EIN (`ein`, e.g. `"04-2104327"`) published
#' by all new NCCS products and the legacy / NODC join key (`EIN2`, e.g.
#' `"EIN-04-2104327"`) used by harmonized CORE marts, the Unified BMF, and NODC
#' `efile_v2_1`. The two carry identical information: `EIN2` is exactly `ein`
#' with a literal `EIN-` prepended, so the bridge is a deterministic string
#' reformat with no lookup table (`nccs-contracts/conventions/ein-format.md`).
#'
#' Both functions route every input through the canonical 9-digit-core
#' normalization (strip non-digits, reject more than nine digits, left-zero-pad
#' to nine, reject the all-zeros placeholder), so they also recover EINs that
#' lost a leading zero on a bare-integer surface (`"42104327"` ->
#' `"04-2104327"`).
#'
#' These helpers are **lenient**: an input that cannot be normalized becomes
#' `NA`, and the call emits a `warning()` reporting how many non-empty inputs
#' were dropped, so silent loss stays visible. EINs are handled as character
#' end to end; never pass numeric storage, which is what drops leading zeros.
#'
#' The round-trip is lossless: `nccs_ein2_to_ein(nccs_ein_to_ein2(e))` returns
#' `e` for every valid `ein`, and the reverse for every valid `EIN2`.
#'
#' @param ein A character (or numeric) vector of EINs in any recognized surface
#'   rendering. Canonical clean form is `XX-XXXXXXX`.
#' @param ein2 A character vector of `EIN2`-form keys (`EIN-XX-XXXXXXX`). The
#'   `EIN-` prefix is optional on input.
#'
#' @return A character vector the same length as the input: `nccs_ein_to_ein2()`
#'   returns `EIN-XX-XXXXXXX`, `nccs_ein2_to_ein()` returns `XX-XXXXXXX`, with
#'   `NA_character_` for un-normalizable elements.
#'
#' @seealso [nccs_normalize_ein()] for coercing arbitrary input to the canonical
#'   `ein` form.
#'
#' @examples
#' nccs_ein_to_ein2("04-2104327")        # "EIN-04-2104327"
#' nccs_ein2_to_ein("EIN-04-2104327")    # "04-2104327"
#'
#' # Leading zeros are preserved through the bridge.
#' nccs_ein2_to_ein("EIN-00-0000004")    # "00-0000004"
#'
#' # Lossless round-trip.
#' nccs_ein2_to_ein(nccs_ein_to_ein2("36-3686904"))  # "36-3686904"
#'
#' @name ein_bridge
NULL

#' @rdname ein_bridge
#' @export
nccs_ein_to_ein2 <- function(ein) {
  if (length(ein) == 0L) return(character(0))

  core <- .ein_core(ein)
  out <- ifelse(
    is.na(core),
    NA_character_,
    paste0("EIN-", substr(core, 1L, 2L), "-", substr(core, 3L, 9L))
  )
  .warn_ein_dropped(ein, out)
  out
}

#' @rdname ein_bridge
#' @export
nccs_ein2_to_ein <- function(ein2) {
  if (length(ein2) == 0L) return(character(0))

  # core() strips the literal "EIN-" prefix along with every other non-digit,
  # so no explicit prefix handling is needed.
  core <- .ein_core(ein2)
  out <- ifelse(
    is.na(core),
    NA_character_,
    paste0(substr(core, 1L, 2L), "-", substr(core, 3L, 9L))
  )
  .warn_ein_dropped(ein2, out)
  out
}
