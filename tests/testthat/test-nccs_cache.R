test_that("nccs_cache_dir returns a character path", {
  d <- nccs_cache_dir()
  expect_type(d, "character")
  expect_length(d, 1L)
})

test_that("nccs_cache_clear on a missing directory returns 0 invisibly", {
  tmp <- file.path(tempdir(), "nccsdata-cache-missing")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  expect_equal(nccs_cache_clear(tmp), 0L)
})

test_that("nccs_cache_clear removes files from an existing directory", {
  tmp <- file.path(tempdir(), "nccsdata-cache-test")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.create(file.path(tmp, "a.parquet"), file.path(tmp, "b.parquet"))
  expect_equal(nccs_cache_clear(tmp), 2L)
  expect_length(list.files(tmp), 0L)
})

test_that(".resolve_cache_arg handles TRUE/FALSE/path", {
  expect_null(nccsdata:::.resolve_cache_arg(FALSE))
  expect_type(nccsdata:::.resolve_cache_arg(TRUE), "character")
  expect_equal(nccsdata:::.resolve_cache_arg("/tmp/x"), "/tmp/x")
  expect_error(nccsdata:::.resolve_cache_arg(1), "TRUE, FALSE, or a single")
  expect_error(nccsdata:::.resolve_cache_arg(NA), "TRUE, FALSE, or a single")
})

test_that(".cache_is_fresh respects mtime threshold", {
  tmp <- tempfile(fileext = ".parquet")
  file.create(tmp)
  on.exit(unlink(tmp), add = TRUE)
  expect_true(nccsdata:::.cache_is_fresh(tmp, 30))
  # Backdate the file to simulate staleness
  Sys.setFileTime(tmp, Sys.time() - 60 * 60 * 24 * 60)
  expect_false(nccsdata:::.cache_is_fresh(tmp, 30))
  expect_false(nccsdata:::.cache_is_fresh(tempfile(), 30))
})

test_that(".bmf_master_source falls back to S3 when download fails", {
  tmp <- file.path(tempdir(), "nccsdata-cache-fail")
  if (dir.exists(tmp)) unlink(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  # Stub out the HTTPS URL to force download.file to fail
  with_mocked <- function() {
    local_url <- function() "https://invalid.invalid/nope.parquet"
    assignInNamespace(".bmf_master_https_url", local_url, ns = "nccsdata")
    on.exit(
      assignInNamespace(
        ".bmf_master_https_url",
        function() "https://nccsdata.s3.amazonaws.com/geocoding/bmf-master/merged/bmf_master_geocoded.parquet",
        ns = "nccsdata"
      ),
      add = TRUE
    )
    expect_warning(
      out <- nccsdata:::.bmf_master_source(tmp, max_age_days = 30),
      "cache download failed"
    )
    expect_equal(out, nccsdata:::.bmf_master_s3_path())
  }
  with_mocked()
})

test_that("nccs_read validates cache_max_age", {
  expect_error(nccs_read(cache_max_age = -1), "non-negative")
  expect_error(nccs_read(cache_max_age = "30"), "non-negative")
  expect_error(nccs_read(cache_max_age = NA_real_), "non-negative")
})
