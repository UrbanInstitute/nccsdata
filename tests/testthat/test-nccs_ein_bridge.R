# Shared contract vectors from nccs-contracts/conventions/ein-format.md Section 5.
# Each row is one EIN across its surface renderings, drawn from live data.
ein_vectors <- data.frame(
  core    = c("042104327",   "363686904",   "382787387",   "000000004"),
  ein     = c("04-2104327",  "36-3686904",  "38-2787387",  "00-0000004"),
  ein2    = c("EIN-04-2104327", "EIN-36-3686904", "EIN-38-2787387", "EIN-00-0000004"),
  padded9 = c("042104327",   "363686904",   "382787387",   "000000004"),
  bare    = c("42104327",    "363686904",   "382787387",   "4"),
  stringsAsFactors = FALSE
)

test_that("nccs_ein_to_ein2 matches the contract vectors from canonical ein", {
  expect_equal(nccs_ein_to_ein2(ein_vectors$ein), ein_vectors$ein2)
})

test_that("nccs_ein2_to_ein matches the contract vectors from EIN2", {
  expect_equal(nccs_ein2_to_ein(ein_vectors$ein2), ein_vectors$ein)
})

test_that("nccs_ein2_to_ein preserves leading zeros", {
  expect_equal(nccs_ein2_to_ein("EIN-00-0000004"), "00-0000004")
})

test_that("the bridge recovers EINs from the lossy bare-integer surface", {
  expect_equal(nccs_ein_to_ein2(ein_vectors$bare), ein_vectors$ein2)
  expect_equal(nccs_ein2_to_ein(ein_vectors$bare), ein_vectors$ein)
})

test_that("round-trip is lossless in both directions", {
  expect_equal(nccs_ein2_to_ein(nccs_ein_to_ein2(ein_vectors$ein)), ein_vectors$ein)
  expect_equal(nccs_ein_to_ein2(nccs_ein2_to_ein(ein_vectors$ein2)), ein_vectors$ein2)
})

test_that("nccs_ein_to_ein2 accepts canonical input unchanged through core", {
  expect_equal(nccs_ein_to_ein2("04-2104327"), "EIN-04-2104327")
})

test_that("nccs_ein2_to_ein normalizes an unpadded malformed EIN2 (lenient/strict-equivalent)", {
  # "EIN-4" -> core "000000004" -> "00-0000004" (Section 5 malformed example).
  expect_equal(nccs_ein2_to_ein("EIN-4"), "00-0000004")
})

test_that("converters reject all-zeros and over-length input with NA", {
  expect_true(is.na(suppressWarnings(nccs_ein_to_ein2("000000000"))))
  expect_true(is.na(suppressWarnings(nccs_ein_to_ein2("EIN-00-0000000"))))
  expect_true(is.na(suppressWarnings(nccs_ein_to_ein2("1234567890"))))   # >9 digits
})

test_that("un-normalizable input becomes NA element-wise", {
  result <- suppressWarnings(nccs_ein_to_ein2(c("04-2104327", "bad", NA)))
  expect_equal(result, c("EIN-04-2104327", NA, NA))
})

test_that("converters warn (not silently drop) when a non-empty value fails", {
  expect_warning(nccs_ein_to_ein2(c("04-2104327", "bad")), "could not be normalized")
  expect_warning(nccs_ein2_to_ein(c("EIN-04-2104327", "xyz")), "could not be normalized")
})

test_that("blank and NA input does not trigger a drop warning", {
  expect_silent(nccs_ein_to_ein2(c("04-2104327", NA, "")))
})

test_that("converters return character(0) on empty input", {
  expect_equal(nccs_ein_to_ein2(character(0)), character(0))
  expect_equal(nccs_ein2_to_ein(character(0)), character(0))
})

test_that("converters return a character vector", {
  expect_type(nccs_ein_to_ein2("04-2104327"), "character")
  expect_type(nccs_ein2_to_ein("EIN-04-2104327"), "character")
})
