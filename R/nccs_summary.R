#' Grouped Count Summaries
#'
#' Produces grouped count summaries from a collected BMF data frame.
#' Useful for generating county-level or subsector-level nonprofit counts.
#'
#' @param data A data frame or tibble, typically the output of [nccs_read()].
#' @param group_by Character vector of column names to group by before
#'   counting. If `NULL` (default), returns the total row count.
#' @param output_csv Optional file path. If provided, writes the result to
#'   a CSV file.
#'
#' @return A tibble with the grouping columns (if any) and a count column `n`.
#'
#' @examples
#' \dontrun{
#' pa <- nccs_read(state = "PA")
#'
#' # Total count
#' nccs_summary(pa)
#'
#' # Count by county
#' nccs_summary(pa, group_by = "geo_county")
#'
#' # Count by county and subsector, save to CSV
#' nccs_summary(pa, group_by = c("geo_county", "nteev2_subsector"),
#'              output_csv = "counts.csv")
#' }
#'
#' @export
nccs_summary <- function(data,
                         group_by = NULL,
                         output_csv = NULL) {

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  # Validate group_by columns exist
  if (!is.null(group_by)) {
    missing_cols <- setdiff(group_by, names(data))
    if (length(missing_cols) > 0) {
      stop(
        "Column(s) not found in data: ", paste(missing_cols, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Build summary
  if (is.null(group_by)) {
    result <- dplyr::tibble(n = nrow(data))
  } else {
    result <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
  }

  # Optional CSV output
  if (!is.null(output_csv)) {
    utils::write.csv(result, file = output_csv, row.names = FALSE)
    message("Results written to ", output_csv)
  }

  result
}
