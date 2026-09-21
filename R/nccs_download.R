#' Download one file into the cache without R's default 60-second limit
#'
#' `utils::download.file()` honours `getOption("timeout")`, which is 60
#' seconds by default. The geocoded Unified BMF is over 600 MB, so on an
#' ordinary connection the download was cut off, the package fell back to
#' reading from S3, and a `.part` file was left behind (backlog Z22).
#'
#' When the `curl` package is installed it is used instead, because it has
#' no such limit. Otherwise the timeout option is raised to at least one
#' hour for the duration of the download and restored afterwards. In either
#' case a leftover partial file is removed before starting and on failure.
#'
#' @param url Public HTTPS URL of the file.
#' @param destfile Final path for the file. The download goes to
#'   `paste0(destfile, ".part")` and is renamed on success.
#' @param use_curl Use the `curl` package (default: whenever it is
#'   installed). Tests pass `FALSE` to exercise the fallback path.
#' @return `TRUE` invisibly on success; errors propagate to the caller.
#' @noRd
.download_to_cache <- function(url, destfile,
                               use_curl = requireNamespace("curl", quietly = TRUE)) {
  partial <- paste0(destfile, ".part")
  if (file.exists(partial)) unlink(partial)

  on_failure <- function(e) {
    if (file.exists(partial)) unlink(partial)
    stop(e)
  }

  tryCatch({
    if (isTRUE(use_curl)) {
      curl::curl_download(url, partial, quiet = TRUE, mode = "wb")
    } else {
      previous_timeout <- getOption("timeout")
      options(timeout = max(3600, previous_timeout, na.rm = TRUE))
      on.exit(options(timeout = previous_timeout), add = TRUE)
      # download.file() can report failure through a non-zero status
      # without raising an error, leaving a partial file behind; treat any
      # non-zero status as a failure so it is never renamed into the cache.
      status <- suppressWarnings(
        utils::download.file(url, partial, mode = "wb", quiet = TRUE)
      )
      if (!identical(as.integer(status), 0L)) {
        stop("download.file() returned status ", status, " for ", url)
      }
    }
    if (!file.rename(partial, destfile)) {
      stop("could not move the downloaded file into place: ", destfile)
    }
    invisible(TRUE)
  }, error = on_failure)
}
