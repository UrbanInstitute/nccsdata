test_that("nccs_deflate base case: amount in base_year returns itself", {
  expect_equal(nccs_deflate(100, year = 2020, base_year = 2020), 100)
})

test_that("nccs_deflate uses standard CPI ratio", {
  cpi <- nccsdata::cpi_u
  c1990 <- cpi$cpi[cpi$year == 1990]
  c2020 <- cpi$cpi[cpi$year == 2020]
  expect_equal(
    nccs_deflate(100, year = 1990, base_year = 2020),
    100 * (c2020 / c1990)
  )
})

test_that("nccs_deflate vectorizes over year", {
  out <- nccs_deflate(c(100, 100, 100),
                      year = c(2000, 2010, 2020),
                      base_year = 2020)
  expect_length(out, 3L)
  # Earlier-year dollars buy more in 2020 terms
  expect_true(out[1] > out[2] && out[2] > out[3])
})

test_that("nccs_deflate recycles scalar year", {
  out <- nccs_deflate(c(100, 200), year = 2010, base_year = 2020)
  expect_length(out, 2L)
  expect_equal(out[2] / out[1], 2)
})

test_that("nccs_deflate warns and returns NA for years outside series", {
  expect_warning(
    out <- nccs_deflate(100, year = 1850, base_year = 2020),
    "CPI not available"
  )
  expect_true(is.na(out))
})

test_that("nccs_deflate errors on bad base_year", {
  expect_error(nccs_deflate(100, year = 2020, base_year = 1850),
               "Series covers")
  expect_error(nccs_deflate(100, year = 2020, base_year = 2020.5),
               "single integer")
})

test_that("nccs_deflate rejects mismatched lengths", {
  expect_error(
    nccs_deflate(c(100, 200, 300), year = c(2000, 2010), base_year = 2020),
    "length 1 or length"
  )
})

test_that("nccs_deflate accepts a custom CPI table", {
  custom <- data.frame(year = c(2010L, 2020L), cpi = c(100, 200))
  expect_equal(
    nccs_deflate(100, year = 2010, base_year = 2020, cpi = custom),
    200
  )
})

test_that("cpi_u dataset is well-formed", {
  expect_s3_class(nccsdata::cpi_u, "tbl_df")
  expect_setequal(names(nccsdata::cpi_u), c("year", "cpi"))
  expect_true(all(diff(nccsdata::cpi_u$year) == 1L))
  expect_true(all(nccsdata::cpi_u$cpi > 0))
})
