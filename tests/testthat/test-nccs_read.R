test_that(".bmf_master_s3_path returns the rolling master URI", {
  expect_equal(
    nccsdata:::.bmf_master_s3_path(),
    "s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet"
  )
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
