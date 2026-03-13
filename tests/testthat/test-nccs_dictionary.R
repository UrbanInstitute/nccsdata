test_that("bmf_dictionary dataset has expected structure", {
  expect_s3_class(bmf_dictionary, "tbl_df")
  expect_equal(names(bmf_dictionary), c("column_name", "description", "type"))
  expect_equal(nrow(bmf_dictionary), 97)
})

test_that("nccs_dictionary() returns full dictionary with no pattern", {
  result <- nccs_dictionary()
  expect_equal(nrow(result), 97)
  expect_equal(names(result), c("column_name", "description", "type"))
})

test_that("nccs_dictionary() filters by pattern", {
  result <- nccs_dictionary("geo")
  expect_true(all(grepl("geo", result$column_name, ignore.case = TRUE)))
  expect_gt(nrow(result), 0)
  expect_lt(nrow(result), 97)
})

test_that("nccs_dictionary() is case-insensitive", {
  result_lower <- nccs_dictionary("ntee")
  result_upper <- nccs_dictionary("NTEE")
  expect_equal(result_lower, result_upper)
})

test_that("nccs_dictionary() returns empty tibble for non-matching pattern", {
  result <- nccs_dictionary("zzzzzzzzz")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("column_name", "description", "type"))
})
