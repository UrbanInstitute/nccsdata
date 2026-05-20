#' Row-Count Coverage of a CORE Series Tier
#'
#' Returns a tibble of `(tax_year, form, n_rows, url)` for every
#' published partition of a tier. Row counts are read from each
#' parquet's footer metadata (no full file scan), but one HTTP request
#' per partition is still issued, so this typically takes 10-60s
#' depending on tier.
#'
#' Missing partitions (e.g. SOI-current `990pf` for 2017-2019) are
#' omitted from the result rather than reported as zero rows. The
#' canonical coverage tables are described in [nccs_core_url()].
#'
#' @param tier One of `"merged"` (default), `"soi"`, or `"legacy"`.
#'
#' @return A tibble with columns `tax_year` (integer), `form`
#'   (character), `n_rows` (double), and `url` (character HTTPS URL of
#'   the parquet).
#'
#' @seealso [nccs_core_url()], [nccs_read_core()].
#'
#' @examples
#' \dontrun{
#' # Coverage of the canonical merged tier
#' cov <- nccs_core_coverage("merged")
#' dplyr::summarise(cov, total = sum(n_rows), .by = form)
#' }
#'
#' @export
nccs_core_coverage <- function(tier = c("merged", "soi", "legacy")) {
  tier  <- match.arg(tier)
  forms <- .core_valid_forms(tier)
  rng   <- .core_year_range(tier)
  years <- rng[1]:rng[2]

  grid <- expand.grid(tax_year = years, form = forms,
                      KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  rows <- vapply(seq_len(nrow(grid)), function(i) {
    .core_partition_num_rows(tier, grid$tax_year[i], grid$form[i])
  }, numeric(1))

  urls <- vapply(seq_len(nrow(grid)), function(i) {
    nccs_core_url(tier, grid$tax_year[i], grid$form[i],
                  format = "parquet", kind = "data")
  }, character(1))

  out <- dplyr::tibble(
    tax_year = as.integer(grid$tax_year),
    form     = grid$form,
    n_rows   = rows,
    url      = urls
  )
  out <- out[!is.na(out$n_rows), , drop = FALSE]
  out[order(out$tax_year, out$form), , drop = FALSE]
}

#' Read num_rows from a CORE partition's parquet footer.
#' Returns NA_real_ if the partition is missing or unreadable.
#' @noRd
.core_partition_num_rows <- function(tier, tax_year, form) {
  uri <- .core_partition_s3_uri(tier, tax_year, form)
  tryCatch({
    ds <- arrow::open_dataset(uri, format = "parquet")
    as.numeric(ds$num_rows)
  }, error = function(e) NA_real_)
}
