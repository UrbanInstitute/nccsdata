test_that(".build_s3_path constructs correct URL", {
  path <- nccsdata:::.build_s3_path("2026_03")
  expect_equal(
    path,
    "s3://nccsdata/geocoding/bmf/2026_03/merged/bmf_2026_03_geocoded.parquet"
  )
})

test_that(".build_s3_path works with different dates", {
  path <- nccsdata:::.build_s3_path("2025_12")
  expect_match(path, "2025_12")
})

test_that("nccs_read validates date format", {
  expect_error(nccs_read(date = "2026-03"), "YYYY_MM")
  expect_error(nccs_read(date = "March2026"), "YYYY_MM")
})

test_that("nccs_read validates state codes", {
  expect_error(nccs_read(state = "ZZ"), "Invalid state")
})

test_that("nccs_read validates ntee_subsector codes", {
  expect_error(nccs_read(ntee_subsector = "INVALID"), "Invalid ntee_subsector")
})

test_that("nccs_read validates exempt_org_type values", {
  expect_error(nccs_read(exempt_org_type = "FAKE"), "Invalid exempt_org_type")
})

# Integration tests requiring network access
test_that("nccs_read returns tibble with state filter", {
  skip_on_cran()
  skip_if_offline()

  result <- nccs_read(state = "DC")
  expect_s3_class(result, "tbl_df")
  expect_true(all(result$org_addr_state == "DC"))
  expect_true("ein" %in% names(result))
})

test_that("nccs_read collect = FALSE returns arrow query", {
  skip_on_cran()
  skip_if_offline()

  result <- nccs_read(state = "DC", collect = FALSE)
  expect_false(is.data.frame(result))
  collected <- dplyr::as_tibble(dplyr::collect(result))
  expect_s3_class(collected, "tbl_df")
})

test_that("nccs_read respects column selection", {
  skip_on_cran()
  skip_if_offline()

  result <- nccs_read(
    state = "DC",
    columns = c("ein", "org_name_display")
  )
  # Should have ein, org_name_display, plus org_addr_state (filter column)
  expect_true("ein" %in% names(result))
  expect_true("org_name_display" %in% names(result))
  expect_true("org_addr_state" %in% names(result))
})
