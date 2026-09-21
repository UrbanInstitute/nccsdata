# Backlog Z22: cache downloads must not be cut off by R's 60-second default,
# and a failed download must not leave a .part file behind.

test_that("a successful download lands at the final path with no .part left", {
  source_file <- withr::local_tempfile(fileext = ".bin")
  writeBin(as.raw(1:200), source_file)
  destination <- file.path(withr::local_tempdir(), "copy.bin")

  expect_true(.download_to_cache(paste0("file://", source_file), destination))
  expect_true(file.exists(destination))
  expect_false(file.exists(paste0(destination, ".part")))
  expect_equal(file.size(destination), 200)
})

test_that("a failed download raises an error and removes the partial file", {
  destination <- file.path(withr::local_tempdir(), "missing.bin")
  writeBin(as.raw(1:3), paste0(destination, ".part"))   # stale leftover from an earlier attempt

  expect_error(
    suppressWarnings(.download_to_cache("file:///definitely/not/here.bin", destination))
  )
  expect_false(file.exists(paste0(destination, ".part")))
  expect_false(file.exists(destination))
})

test_that("without curl, the timeout option is raised only for the download and then restored", {
  skip_if(requireNamespace("curl", quietly = TRUE), "curl is installed; the fallback path is not used")
  withr::local_options(timeout = 60)
  source_file <- withr::local_tempfile(fileext = ".bin")
  writeBin(as.raw(1:10), source_file)
  destination <- file.path(withr::local_tempdir(), "copy.bin")
  .download_to_cache(paste0("file://", source_file), destination)
  expect_equal(getOption("timeout"), 60)
})
