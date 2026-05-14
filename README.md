
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nccsdata <img src="hex/nccsdata.svg" align="right" height="139" alt="nccsdata hex logo" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/UrbanInstitute/nccsdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UrbanInstitute/nccsdata/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/UrbanInstitute/nccsdata/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/UrbanInstitute/nccsdata/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

nccsdata provides tools to download, filter, and analyze nonprofit
organization data from the [National Center for Charitable
Statistics](https://nccs.urban.org/) (NCCS). It reads IRS Business
Master File (BMF) data stored as parquet files in a public S3 bucket,
with support for predicate-pushdown filtering by state, county, NTEE
subsector, and exempt organization type.

> **Note:** This is version 2.0.0, a ground-up rewrite of the package.
> The v1 API (`get_data()`, `preview_sample()`, `parse_ntee()`) has been
> replaced. See the [migration section](#migrating-from-v1) below.

## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("UrbanInstitute/nccsdata")
```

## Usage

### Reading BMF data

`nccs_read()` downloads BMF data from S3 with optional filters.
Filtering happens at the Arrow level via predicate pushdown, so only
matching rows are read into memory.

``` r
library(nccsdata)

# All Pennsylvania nonprofits (default columns)
pa <- nccs_read(state = "PA")

# Arts nonprofits in New York
ny_arts <- nccs_read(state = "NY", ntee_subsector = "ART")

# 501(c)(3) private foundations in California
ca_pf <- nccs_read(state = "CA", org_type = "private_foundation")

# All 501(c)(3) public charities (the most common analyst cut)
pc <- nccs_read(org_type = "public_charity")

# Select specific columns
pa_slim <- nccs_read(
  state = "PA",
  columns = c("ein", "org_name_display", "geo_county", "income_amount")
)

# Lazy query for custom dplyr pipelines
query <- nccs_read(state = "PA", collect = FALSE)
result <- query |>
  dplyr::filter(geo_county == "Lackawanna County") |>
  dplyr::collect()
```

### Column types

The upstream parquet stores most columns as `character` (vintage
stacking requires it). `nccs_read()` coerces known financial, date,
and indicator columns to their natural types on the collected tibble
by default — pass `coerce = FALSE` to opt out. ZIPs and other ID-like
codes are intentionally left as `character`.

### Local caching

`nccs_read()` caches the geocoded master parquet (hundreds of MB) in
`tools::R_user_dir("nccsdata", "cache")` so subsequent calls in the
same or future sessions skip the S3 download. The cached copy refreshes
once it's more than `cache_max_age` days old (30 by default; the
upstream master is rebuilt monthly). Pass `cache = FALSE` to always
read from S3, or use `nccs_cache_clear()` to force the next call to
re-download.

``` r
nccs_cache_dir()
nccs_cache_clear()  # force re-download next time
```

### Reading historical vintages

`nccs_read()` reads the rolling geocoded master. For a specific dated
monthly snapshot — useful for reproducible research — use
`nccs_vintage_url()` to get the S3 URI, then read the CSV directly.
Vintage schemas differ by month and across the modern/legacy seam, so
inspect the matching data dictionary before filtering.

``` r
uri  <- nccs_vintage_url("2023_07")
dict <- arrow::read_csv_arrow(nccs_vintage_url("2023_07", kind = "dictionary"))
bmf_2023_07 <- arrow::read_csv_arrow(uri)
```

### Summarizing data

`nccs_summary()` produces grouped count summaries from a collected data
frame.

``` r
pa <- nccs_read(state = "PA")

# Total count
nccs_summary(pa)

# Count by county
nccs_summary(pa, group_by = "geo_county")

# Count by county and subsector, export to CSV
nccs_summary(pa, group_by = c("geo_county", "nteev2_subsector"),
             output_csv = "pa_counts.csv")
```

### Discovering valid filter values

`nccs_catalog()` lists valid values for `nccs_read()` filters without
any network calls.

``` r
nccs_catalog("state")
nccs_catalog("ntee_subsector")
nccs_catalog("exempt_org_type")

# Pass `labels = TRUE` for a code + description tibble, sourced from the
# bundled BMF lookup tables.
nccs_catalog("ntee_subsector", labels = TRUE)
nccs_catalog("foundation_code", labels = TRUE)
```

### Cleaning external data

The BMF returned by `nccs_read()` is already normalized upstream, but
two helpers are exposed for users joining external CSVs or API responses
against it:

``` r
# Coerce EINs in any format to canonical XX-XXXXXXX
nccs_normalize_ein(c("123456789", "12-3456789", 1234567))
#> [1] "12-3456789" "12-3456789" "00-1234567"

# Coerce IRS binary-indicator columns to logical
nccs_as_indicator(c("Y", "N", "1", "2"))
#> [1]  TRUE FALSE  TRUE FALSE

# e-file indicator accepts E/P (2015, 2018+) and Y/N (2016-2017)
nccs_as_indicator(c("E", "P", "Y", "N"), scheme = "efile")
```

### Browsing the data dictionary

`nccs_dictionary()` returns a tibble describing all BMF columns, with
optional pattern filtering.

``` r
# All columns
nccs_dictionary()

# Find geocoding-related columns
nccs_dictionary("geo")

# Find NTEE-related columns
nccs_dictionary("ntee")
```

## Migrating from v1

| v1 function                       | v2 replacement                   |
|-----------------------------------|----------------------------------|
| `get_data()`                      | `nccs_read()`                    |
| `preview_sample()`                | `nccs_summary()`                 |
| `ntee_preview()` / `parse_ntee()` | `nccs_catalog("ntee_subsector")` |

Key changes:

- Data source moved from legacy Core/BMF CSVs to geocoded BMF parquet
  files on S3.
- Filtering now uses Arrow predicate pushdown instead of downloading
  full files.
- Dependencies reduced from 12 packages to 3 (`arrow`, `dplyr`,
  `utils`).

## Documentation

Full documentation is available at
<https://urbaninstitute.github.io/nccsdata/>.

## Getting help

- Browse the [getting started
  vignette](https://urbaninstitute.github.io/nccsdata/articles/getting-started.html)
- Open an issue on
  [GitHub](https://github.com/UrbanInstitute/nccsdata/issues)
- Contact the maintainer at `tpoongundranar@urban.org`
