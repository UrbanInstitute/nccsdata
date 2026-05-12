test_that("nccs_as_indicator yn scheme maps standard tokens", {
  result <- nccs_as_indicator(c("Y", "y", "1", "T", "TRUE", "true"))
  expect_true(all(result))

  result <- nccs_as_indicator(c("N", "n", "0", "F", "FALSE", "false"))
  expect_true(all(!result))
})

test_that("nccs_as_indicator yn scheme treats 2 as FALSE", {
  expect_equal(nccs_as_indicator(c("1", "2")), c(TRUE, FALSE))
})

test_that("nccs_as_indicator efile scheme accepts E/P, Y/N, and 1/0", {
  result <- nccs_as_indicator(c("E", "P", "Y", "N", "1", "0"), scheme = "efile")
  expect_equal(result, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("nccs_as_indicator propagates NA without warning", {
  expect_silent(out <- nccs_as_indicator(c("Y", NA, "N")))
  expect_equal(out, c(TRUE, NA, FALSE))
})

test_that("nccs_as_indicator emits a single warning listing distinct unknowns", {
  expect_warning(
    out <- nccs_as_indicator(c("Y", "MAYBE", "huh", "MAYBE", "N")),
    "MAYBE.*huh|huh.*MAYBE"
  )
  expect_equal(out, c(TRUE, NA, NA, NA, FALSE))
})

test_that("nccs_as_indicator returns logical(0) on empty input", {
  expect_equal(nccs_as_indicator(character(0)), logical(0))
  expect_equal(nccs_as_indicator(integer(0)),   logical(0))
})

test_that("nccs_as_indicator passes logical input through unchanged", {
  x <- c(TRUE, FALSE, NA)
  expect_identical(nccs_as_indicator(x), x)
})

test_that("nccs_as_indicator handles numeric input", {
  expect_equal(nccs_as_indicator(c(1, 0, 2)), c(TRUE, FALSE, FALSE))
})

test_that("nccs_as_indicator errors on unknown scheme", {
  expect_error(nccs_as_indicator("Y", scheme = "bogus"))
})

test_that("nccs_as_indicator trims whitespace before matching", {
  expect_equal(nccs_as_indicator(c(" Y ", "  N")), c(TRUE, FALSE))
})

test_that("nccs_as_indicator treats empty string as NA without warning", {
  expect_silent(out <- nccs_as_indicator(c("Y", "", "N")))
  expect_equal(out, c(TRUE, NA, FALSE))
})
