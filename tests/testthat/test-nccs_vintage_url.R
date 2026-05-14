test_that("nccs_vintage_url builds modern data URI", {
  expect_equal(
    nccs_vintage_url("2023_07"),
    "s3://nccsdata/processed/bmf/2023_07/bmf_2023_07_processed.csv"
  )
})

test_that("nccs_vintage_url builds modern dictionary URI", {
  expect_equal(
    nccs_vintage_url("2023_09", kind = "dictionary"),
    "s3://nccsdata/processed/bmf/2023_09/bmf_2023_09_data_dictionary.csv"
  )
})

test_that("nccs_vintage_url builds legacy data URI", {
  expect_equal(
    nccs_vintage_url("1999_12", legacy = TRUE),
    "s3://nccsdata/processed/bmf-legacy/1999_12/bmf_legacy_1999_12_processed.csv"
  )
})

test_that("nccs_vintage_url builds legacy dictionary URI", {
  expect_equal(
    nccs_vintage_url("2003_01", kind = "dictionary", legacy = TRUE),
    "s3://nccsdata/processed/bmf-legacy/2003_01/bmf_legacy_2003_01_data_dictionary.csv"
  )
})

test_that("nccs_vintage_url validates inputs", {
  expect_error(nccs_vintage_url("2023-07"), "'YYYY_MM'")
  expect_error(nccs_vintage_url("202307"), "'YYYY_MM'")
  expect_error(nccs_vintage_url(c("2023_07", "2023_08")), "'YYYY_MM'")
  expect_error(nccs_vintage_url("2023_07", kind = "schema"), "should be one of")
  expect_error(nccs_vintage_url("2023_07", legacy = "yes"), "TRUE or FALSE")
})
