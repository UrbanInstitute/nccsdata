#' Coerce IRS Binary-Indicator Columns to Logical
#'
#' IRS Business Master File extracts encode binary fields using inconsistent
#' tokens that vary by vintage. This helper coerces such a column to a logical
#' vector under one of two well-known schemes.
#'
#' @section Schemes:
#'
#' `"yn"` — general yes/no indicator columns (often the `_cd` suffix in
#' upstream data).
#' \itemize{
#'   \item TRUE:  `Y`, `y`, `1`, `T`, `TRUE`, `true`, `True`
#'   \item FALSE: `N`, `n`, `0`, `2`, `F`, `FALSE`, `false`, `False`
#' }
#' Note that `"2"` maps to FALSE: the IRS shifted some binary fields to a
#' 1 (= yes) / 2 (= no) encoding in recent vintages (e.g. some 2022/2023
#' 990 columns).
#'
#' `"efile"` — the IRS e-file indicator, whose accepted tokens vary by
#' filing year:
#' \itemize{
#'   \item 2015 990 / 990-EZ used `E` (electronic) / `P` (paper).
#'   \item 2016 and 2017 990 / 990-EZ switched to `Y` / `N`.
#'   \item 2018 onwards returned to `E` / `P`.
#' }
#' All three encodings are accepted:
#' \itemize{
#'   \item TRUE:  `E`, `e`, `Y`, `y`, `1`, `T`, `TRUE`, `true`, `True`
#'   \item FALSE: `P`, `p`, `N`, `n`, `0`, `F`, `FALSE`, `false`, `False`
#' }
#'
#' Tokens outside the accepted set become `NA`, with a single `warning()`
#' that lists the distinct unknown values (not one per element). `NA`
#' input is propagated silently.
#'
#' Pure base-R, no side effects, no in-place mutation. Designed for use
#' on external data the user is joining against [nccs_read()] output —
#' columns returned by `nccs_read()` are already cleaned upstream.
#'
#' @param x A character, numeric, or logical vector.
#' @param scheme One of `"yn"` (default) or `"efile"`.
#'
#' @return A logical vector the same length as `x`.
#'
#' @examples
#' nccs_as_indicator(c("Y", "N", "1", "2", NA))
#' # [1]  TRUE FALSE  TRUE FALSE    NA
#'
#' nccs_as_indicator(c("E", "P", "Y", "N"), scheme = "efile")
#' # [1]  TRUE FALSE  TRUE FALSE
#'
#' @export
nccs_as_indicator <- function(x, scheme = c("yn", "efile")) {
  scheme <- match.arg(scheme)

  if (length(x) == 0L) return(logical(0))
  if (is.logical(x))   return(x)

  true_tokens <- c("Y", "y", "1", "T", "TRUE", "true", "True")
  false_tokens <- c("N", "n", "0", "2", "F", "FALSE", "false", "False")
  if (scheme == "efile") {
    true_tokens  <- c("E", "e", true_tokens)
    false_tokens <- c("P", "p", false_tokens)
  }

  s <- trimws(as.character(x))
  out <- rep(NA, length(s))
  out[s %in% true_tokens]  <- TRUE
  out[s %in% false_tokens] <- FALSE

  unknown_mask <- is.na(out) & !is.na(s) & nzchar(s)
  if (any(unknown_mask)) {
    unknown_vals <- unique(s[unknown_mask])
    warning(
      "nccs_as_indicator: ", length(unknown_vals),
      " unknown value(s) coerced to NA: ",
      paste(shQuote(unknown_vals), collapse = ", "),
      call. = FALSE
    )
  }

  as.logical(out)
}
