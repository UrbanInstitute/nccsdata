test_that(".core_partition_s3_uri builds canonical URIs", {
  expect_equal(
    nccsdata:::.core_partition_s3_uri("merged", 2020L, "990combined"),
    "s3://nccsdata/processed_merged/core/2020/990combined/core_2020_990combined.parquet"
  )
  expect_equal(
    nccsdata:::.core_partition_s3_uri("soi", 2020L, "990"),
    "s3://nccsdata/processed/core/2020/990/core_2020_990.parquet"
  )
  expect_equal(
    nccsdata:::.core_partition_s3_uri("legacy", 1995L, "990combined"),
    "s3://nccsdata/processed_legacy/core/1995/990combined/core_1995_990combined.parquet"
  )
})

test_that("nccs_read_core rejects bad columns argument", {
  expect_error(
    nccs_read_core("merged", 2020, "990combined", columns = 1:3),
    "`columns` must be a character"
  )
  expect_error(
    nccs_read_core("merged", 2020, "990combined", columns = c("ein", NA)),
    "`columns` must be a character"
  )
})

test_that("nccs_read_core rejects invalid partitions", {
  expect_error(nccs_read_core("legacy", 2000, "990ez"), "not published")
  expect_error(nccs_read_core("soi", 2010, "990"), "outside tier")
})

test_that("nccs_read_core rejects bad tax_year vectors", {
  expect_error(nccs_read_core("merged", integer(0), "990combined"),
               "non-empty integer")
  expect_error(nccs_read_core("merged", c(2020, NA), "990combined"),
               "non-empty integer")
  expect_error(nccs_read_core("merged", 2020.5, "990combined"),
               "non-empty integer")
})

test_that("nccs_read_core validates every year in a vector", {
  # 2010 is outside SOI range (2012-2024) — must error before any network
  expect_error(nccs_read_core("soi", c(2015, 2010), "990"),
               "outside tier")
})

test_that(".format_bytes renders SI-ish units", {
  expect_equal(nccsdata:::.format_bytes(0),         "size unknown")
  expect_equal(nccsdata:::.format_bytes(NA_real_),  "size unknown")
  expect_equal(nccsdata:::.format_bytes(512),       "512.0 B")
  expect_equal(nccsdata:::.format_bytes(1536),      "1.5 KB")
  expect_equal(nccsdata:::.format_bytes(2.5 * 1024^3), "2.5 GB")
})

# Integration test requiring network access
test_that("nccs_read_core returns tibble with column projection", {
  skip_on_cran()
  skip_if_offline()

  result <- nccs_read_core(
    "merged", 2020, "990combined",
    columns = c("ein", "tax_period"),
    cache = FALSE
  )
  expect_s3_class(result, "tbl_df")
  expect_setequal(names(result), c("ein", "tax_period"))
  expect_gt(nrow(result), 0)
})

test_that("nccs_read_core collect = FALSE returns lazy query", {
  skip_on_cran()
  skip_if_offline()

  q <- nccs_read_core("merged", 2020, "990combined",
                      columns = c("ein", "tax_period"),
                      cache = FALSE, collect = FALSE)
  expect_false(is.data.frame(q))
  collected <- dplyr::as_tibble(dplyr::collect(q))
  expect_s3_class(collected, "tbl_df")
})
