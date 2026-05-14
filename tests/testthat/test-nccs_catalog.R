test_that("nccs_catalog returns NTEE subsectors as inline code - description", {
  result <- nccs_catalog("ntee_subsector")
  expect_type(result, "character")
  expect_length(result, 12)
  expect_true(any(grepl("^ART - ", result)))
  expect_true(any(grepl("^EDU - ", result)))
  expect_true(any(grepl("^UNU - ", result)))
})

test_that("nccs_read accepts inline subsector strings from nccs_catalog", {
  inline <- nccs_catalog("ntee_subsector")
  uni_inline <- inline[grepl("^UNI - ", inline)]
  expect_equal(nccsdata:::.resolve_ntee_subsector(uni_inline), "UNI")
})

test_that("nccs_catalog returns state codes including territories", {
  result <- nccs_catalog("state")
  expect_type(result, "character")
  expect_true(all(c("PA", "DC", "PR", "GU", "VI", "AS", "MP") %in% result))
  expect_length(result, 56)
})

test_that("nccs_catalog returns exempt org types", {
  result <- nccs_catalog("exempt_org_type")
  expect_type(result, "character")
  expect_length(result, 33)
  expect_true(any(grepl("501\\(c\\)\\(3\\)", result)))
})

test_that("nccs_catalog errors on invalid field", {
  expect_error(nccs_catalog("invalid_field"))
})

test_that("nccs_catalog errors on non-logical labels", {
  expect_error(nccs_catalog("state", labels = "yes"))
  expect_error(nccs_catalog("state", labels = NA))
})

test_that("nccs_catalog labels = TRUE returns tibble for ntee_subsector", {
  result <- nccs_catalog("ntee_subsector", labels = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("code", "description"))
  expect_true("ART" %in% result$code)
  expect_true(any(grepl("Arts", result$description)))
})

test_that("nccs_catalog labels = TRUE pairs state abbrs with names", {
  result <- nccs_catalog("state", labels = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("code", "description"))
  expect_equal(nrow(result), 56)
  expect_equal(result$description[result$code == "PA"], "Pennsylvania")
  expect_equal(result$description[result$code == "DC"], "District of Columbia")
})

test_that("nccs_catalog labels = TRUE decodes foundation_code", {
  result <- nccs_catalog("foundation_code", labels = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("code", "description"))
  expect_true(nrow(result) > 0)
})

test_that("nccs_catalog labels = TRUE decodes affiliation_code", {
  result <- nccs_catalog("affiliation_code", labels = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("code", "description"))
  expect_true(nrow(result) > 0)
})

test_that("nccs_catalog labels = TRUE on exempt_org_type pairs codes with descriptions", {
  result <- nccs_catalog("exempt_org_type", labels = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("code", "description"))
  expect_true(any(grepl("501\\(c\\)\\(3\\)", result$description)))
})

test_that("nccs_catalog subsection_code returns codes as character", {
  codes <- nccs_catalog("subsection_code")
  expect_type(codes, "character")
  expect_true(length(codes) > 0)
})

test_that("nccs_catalog returns NTEE major group letters", {
  result <- nccs_catalog("ntee_major_group")
  expect_equal(result, LETTERS)
})

test_that("nccs_catalog labels = TRUE decodes ntee_major_group", {
  result <- nccs_catalog("ntee_major_group", labels = TRUE)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("code", "description"))
  expect_equal(nrow(result), 26)
  expect_equal(result$description[result$code == "A"], "Arts, Culture, and Humanities")
  expect_equal(result$description[result$code == "Z"], "Unknown")
})
