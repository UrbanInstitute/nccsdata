#' Manage the Local BMF Cache
#'
#' `nccs_read()` downloads the geocoded master BMF parquet (hundreds of MB)
#' from S3 on first use and reuses a local copy on subsequent calls. These
#' helpers expose the cache directory used by default and let you wipe it.
#'
#' The default location is `tools::R_user_dir("nccsdata", "cache")`. The
#' cached file is considered stale after `cache_max_age` days (30 by
#' default; the upstream master is rebuilt monthly).
#'
#' @return `nccs_cache_dir()` returns the cache directory as a character
#'   string. `nccs_cache_clear()` returns the number of files removed,
#'   invisibly.
#'
#' @examples
#' nccs_cache_dir()
#'
#' \dontrun{
#' # Force a refresh on the next nccs_read() call
#' nccs_cache_clear()
#' }
#'
#' @export
nccs_cache_dir <- function() {
  tools::R_user_dir("nccsdata", which = "cache")
}

#' @rdname nccs_cache_dir
#' @param dir Directory to clear. Defaults to [nccs_cache_dir()].
#' @export
nccs_cache_clear <- function(dir = nccs_cache_dir()) {
  if (!dir.exists(dir)) return(invisible(0L))
  files <- list.files(dir, full.names = TRUE)
  ok <- file.remove(files)
  invisible(sum(ok))
}
