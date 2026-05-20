test_that("nccs_core_columns rejects invalid pattern", {
  expect_error(
    nccs_core_columns("merged", 2020, "990combined", pattern = c("a", "b")),
    "single string"
  )
  expect_error(
    nccs_core_columns("merged", 2020, "990combined", pattern = NA_character_),
    "single string"
  )
})

test_that("nccs_core_columns rejects invalid partitions", {
  expect_error(nccs_core_columns("legacy", 2000, "990"), "not published")
})

test_that("nccs_core_columns returns dictionary tibble", {
  skip_on_cran()
  skip_if_offline()

  dict <- nccs_core_columns("merged", 2020, "990combined")
  expect_s3_class(dict, "tbl_df")
  expect_true("harmonized_name" %in% names(dict))
  expect_gt(nrow(dict), 0)
})

test_that("nccs_core_columns filters by pattern", {
  skip_on_cran()
  skip_if_offline()

  full <- nccs_core_columns("merged", 2020, "990combined")
  hit  <- nccs_core_columns("merged", 2020, "990combined",
                            pattern = "revenue")
  expect_lt(nrow(hit), nrow(full))
  expect_gt(nrow(hit), 0)
})
