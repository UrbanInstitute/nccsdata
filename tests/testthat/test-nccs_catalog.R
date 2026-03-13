test_that("nccs_catalog returns correct NTEE subsector codes", {
  result <- nccs_catalog("ntee_subsector")
  expect_type(result, "character")
  expect_length(result, 12)
  expect_true("ART" %in% result)
  expect_true("EDU" %in% result)
  expect_true("UNU" %in% result)
})

test_that("nccs_catalog returns state codes including territories", {

  result <- nccs_catalog("state")
  expect_type(result, "character")
  expect_true("PA" %in% result)
  expect_true("DC" %in% result)
  expect_true("PR" %in% result)
  expect_true("GU" %in% result)
  expect_true("VI" %in% result)
  expect_true("AS" %in% result)
  expect_true("MP" %in% result)
  expect_length(result, 56) # 50 states + DC, PR, GU, VI, AS, MP
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
