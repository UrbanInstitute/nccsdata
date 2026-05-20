test_that("nccs_core_url builds merged parquet data URL", {
  expect_equal(
    nccs_core_url("merged", 2020, "990combined"),
    paste0("https://nccsdata.s3.amazonaws.com/processed_merged/core/",
           "2020/990combined/core_2020_990combined.parquet")
  )
})

test_that("nccs_core_url builds SOI dictionary URL", {
  expect_equal(
    nccs_core_url("soi", 2020, "990", kind = "dictionary"),
    paste0("https://nccsdata.s3.amazonaws.com/processed/core/",
           "2020/990/core_2020_990_dictionary.parquet")
  )
})

test_that("nccs_core_url builds legacy CSV URL", {
  expect_equal(
    nccs_core_url("legacy", 1995, "990combined", format = "csv"),
    paste0("https://nccsdata.s3.amazonaws.com/processed_legacy/core/",
           "1995/990combined/core_1995_990combined.csv")
  )
})

test_that("nccs_core_url rejects out-of-range tax_year", {
  expect_error(nccs_core_url("soi", 2010, "990"), "outside tier")
  expect_error(nccs_core_url("legacy", 2012, "990combined"), "outside tier")
  expect_error(nccs_core_url("merged", 1986, "990combined"), "outside tier")
})

test_that("nccs_core_url rejects forms not published in tier", {
  expect_error(nccs_core_url("legacy", 2000, "990"), "not published")
  expect_error(nccs_core_url("legacy", 2000, "990ez"), "not published")
  expect_error(nccs_core_url("merged", 2020, "990ez"), "not published")
})

test_that("nccs_core_url rejects bad input types", {
  expect_error(nccs_core_url("merged", "2020", "990combined"), "tax_year")
  expect_error(nccs_core_url("merged", c(2019, 2020), "990combined"), "tax_year")
  expect_error(nccs_core_url("merged", 2020.5, "990combined"), "tax_year")
  expect_error(nccs_core_url("merged", 2020, c("990combined", "990pf")), "form")
})
