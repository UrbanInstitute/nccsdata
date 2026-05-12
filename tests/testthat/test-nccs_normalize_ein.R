test_that("nccs_normalize_ein normalizes common input formats", {
  result <- nccs_normalize_ein(c("123456789", "12-3456789", 123456789))
  expect_equal(result, rep("12-3456789", 3))
})

test_that("nccs_normalize_ein restores leading zeros lost in numeric coercion", {
  expect_equal(nccs_normalize_ein("1234567"), "00-1234567")
  expect_equal(nccs_normalize_ein(1234567),   "00-1234567")
})

test_that("nccs_normalize_ein strips embedded whitespace and punctuation", {
  expect_equal(nccs_normalize_ein(" 12-3456789 "), "12-3456789")
  expect_equal(nccs_normalize_ein("12 3456789"),   "12-3456789")
})

test_that("nccs_normalize_ein returns NA for un-coercible input", {
  expect_true(is.na(nccs_normalize_ein(NA)))
  expect_true(is.na(nccs_normalize_ein("abc")))
  expect_true(is.na(nccs_normalize_ein("")))
  expect_true(is.na(nccs_normalize_ein("1234567890")))  # > 9 digits
})

test_that("nccs_normalize_ein returns character(0) on empty input", {
  expect_equal(nccs_normalize_ein(character(0)), character(0))
  expect_equal(nccs_normalize_ein(integer(0)),   character(0))
})

test_that("nccs_normalize_ein handles mixed-validity vector element-wise", {
  result <- nccs_normalize_ein(c("12-3456789", "bad", NA, "987654321"))
  expect_equal(result, c("12-3456789", NA, NA, "98-7654321"))
})

test_that("nccs_normalize_ein returns a character vector", {
  expect_type(nccs_normalize_ein(c("1", "2")), "character")
})
