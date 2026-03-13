test_that("nccs_summary returns total count with no grouping", {
  df <- dplyr::tibble(
    ein = c("12-3456789", "98-7654321", "11-1111111"),
    geo_county = c("County A", "County B", "County A")
  )
  result <- nccs_summary(df)
  expect_equal(result$n, 3)
  expect_equal(names(result), "n")
})

test_that("nccs_summary returns grouped counts", {
  df <- dplyr::tibble(
    ein = c("12-3456789", "98-7654321", "11-1111111"),
    geo_county = c("County A", "County B", "County A"),
    nteev2_subsector = c("ART", "EDU", "ART")
  )
  result <- nccs_summary(df, group_by = "geo_county")
  expect_equal(nrow(result), 2)
  expect_true("n" %in% names(result))
  expect_true("geo_county" %in% names(result))
  expect_equal(result$n[result$geo_county == "County A"], 2)
})

test_that("nccs_summary supports multiple grouping columns", {
  df <- dplyr::tibble(
    geo_county = c("A", "A", "B"),
    nteev2_subsector = c("ART", "EDU", "ART")
  )
  result <- nccs_summary(df, group_by = c("geo_county", "nteev2_subsector"))
  expect_equal(nrow(result), 3)
})

test_that("nccs_summary errors on invalid column name", {
  df <- dplyr::tibble(ein = "12-3456789")
  expect_error(
    nccs_summary(df, group_by = "nonexistent"),
    "not found"
  )
})

test_that("nccs_summary errors on non-data.frame input", {
  expect_error(nccs_summary("not a dataframe"), "data frame")
})

test_that("nccs_summary writes CSV when output_csv is specified", {
  df <- dplyr::tibble(
    ein = c("12-3456789", "98-7654321"),
    geo_county = c("County A", "County B")
  )
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))

  result <- nccs_summary(df, group_by = "geo_county", output_csv = tmp)
  expect_true(file.exists(tmp))

  written <- utils::read.csv(tmp)
  expect_equal(nrow(written), 2)
  expect_true("n" %in% names(written))
})
