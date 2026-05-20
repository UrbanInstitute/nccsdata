test_that("nccs_core_coverage rejects invalid tier", {
  expect_error(nccs_core_coverage("garbage"), "should be one of")
})

test_that("nccs_core_coverage returns expected shape", {
  skip_on_cran()
  skip_if_offline()

  cov <- nccs_core_coverage("legacy")
  expect_s3_class(cov, "tbl_df")
  expect_setequal(names(cov), c("tax_year", "form", "n_rows", "url"))
  expect_true(is.integer(cov$tax_year))
  expect_true(all(cov$n_rows > 0))
  expect_true(all(cov$form %in% c("990combined", "990pf")))
  # Legacy spans 1987-2011 = 25 years × 2 forms = 50 partitions max,
  # minus known gaps (1993/990pf has only ~11k rows but is present).
  expect_gt(nrow(cov), 30)
})
