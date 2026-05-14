test_that(".coerce_bmf_columns casts numeric/date/indicator columns", {
  df <- dplyr::tibble(
    ein               = c("12-3456789", "98-7654321"),
    asset_amount      = c("1000", "2000.5"),
    revenue_amount    = c("500", NA_character_),
    geo_score         = c("0.95", "0.5"),
    ruling_date       = c("2010-05-01", "1999-12-31"),
    tax_period_ymd    = c("2023-06-30", NA_character_),
    org_addr_is_po_box = c("True", "False"),
    subsection_code   = c("03", "04")
  )
  out <- nccsdata:::.coerce_bmf_columns(df)
  expect_type(out$asset_amount, "double")
  expect_equal(out$asset_amount, c(1000, 2000.5))
  expect_true(is.na(out$revenue_amount[2]))
  expect_s3_class(out$ruling_date, "Date")
  expect_equal(format(out$ruling_date[1]), "2010-05-01")
  expect_true(is.na(out$tax_period_ymd[2]))
  expect_type(out$org_addr_is_po_box, "logical")
  expect_equal(out$org_addr_is_po_box, c(TRUE, FALSE))
  # Code columns left as character; ein untouched
  expect_type(out$subsection_code, "character")
  expect_type(out$ein, "character")
})

test_that(".coerce_bmf_columns skips missing columns", {
  df <- dplyr::tibble(ein = "12-3456789", asset_amount = "100")
  out <- nccsdata:::.coerce_bmf_columns(df)
  expect_named(out, c("ein", "asset_amount"))
  expect_type(out$asset_amount, "double")
})

test_that("nccs_read validates `coerce`", {
  expect_error(nccs_read(coerce = "yes"), "TRUE or FALSE")
  expect_error(nccs_read(coerce = NA), "TRUE or FALSE")
  expect_error(nccs_read(coerce = c(TRUE, TRUE)), "TRUE or FALSE")
})

test_that(".bmf_coerce_spec exposes the documented column lists", {
  spec <- nccsdata:::.bmf_coerce_spec()
  expect_true(all(c("asset_amount", "revenue_amount", "income_amount") %in% spec$numeric))
  expect_true(all(c("ruling_date", "tax_period_ymd") %in% spec$date))
  expect_true("org_addr_is_po_box" %in% spec$indicator)
})
