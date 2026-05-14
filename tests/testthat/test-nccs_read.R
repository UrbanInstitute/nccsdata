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

test_that("nccs_read validates ntee_major_group letters", {
  expect_error(nccs_read(ntee_major_group = "AA"), "Invalid ntee_major_group")
  expect_error(nccs_read(ntee_major_group = "a"), "Invalid ntee_major_group")
})

test_that("nccs_read accepts ntee_subsector by code or name", {
  expect_equal(nccsdata:::.resolve_ntee_subsector("UNI"), "UNI")
  expect_equal(nccsdata:::.resolve_ntee_subsector("Universities"), "UNI")
  expect_equal(nccsdata:::.resolve_ntee_subsector("universities"), "UNI")
  expect_equal(
    nccsdata:::.resolve_ntee_subsector(c("UNI", "Environment and Animals")),
    c("UNI", "ENV")
  )
  expect_error(
    nccsdata:::.resolve_ntee_subsector("Not A Real Subsector"),
    "Invalid ntee_subsector"
  )
})

test_that("nccs_read validates size_min / size_max", {
  expect_error(nccs_read(size_min = "10000"), "`size_min` must be a single numeric")
  expect_error(nccs_read(size_max = c(1, 2)), "`size_max` must be a single numeric")
  expect_error(
    nccs_read(size_min = 1000, size_max = 500),
    "`size_min` must be <= `size_max`"
  )
})

test_that("nccs_read validates size_metric", {
  expect_error(nccs_read(size_metric = "expenses"), "should be one of")
})

test_that("nccs_read validates org_type", {
  expect_error(nccs_read(org_type = "501c4"), "should be one of")
})

test_that("nccs_read validates min_last_year", {
  expect_error(nccs_read(min_last_year = "2024"), "must be a single integer")
  expect_error(nccs_read(min_last_year = 2024.5), "must be a single integer")
  expect_error(nccs_read(min_last_year = c(2023, 2024)), "must be a single integer")
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

test_that("nccs_read filters by ntee_major_group", {
  skip_on_cran()
  skip_if_offline()

  result <- nccs_read(state = "DC", ntee_major_group = "B")
  expect_true(all(result$ntee_code_major_group == "B"))
})

test_that("nccs_read filters by size range", {
  skip_on_cran()
  skip_if_offline()

  result <- nccs_read(
    state = "DC",
    size_metric = "revenue",
    size_min = 1e6,
    size_max = 1e7
  )
  amts <- suppressWarnings(as.numeric(result$revenue_amount))
  expect_true(all(amts >= 1e6 & amts <= 1e7))
})

test_that("nccs_read filters by min_last_year", {
  skip_on_cran()
  skip_if_offline()

  result <- nccs_read(state = "DC", min_last_year = 2024)
  expect_true(all(result$last_year_in_bmf >= 2024))
})
