# Getting Started with nccsdata

## Overview

The `nccsdata` package provides access to nonprofit organization data
from the National Center for Charitable Statistics (NCCS). It reads IRS
Business Master File (BMF) data stored as parquet files in a public S3
bucket, with support for efficient filtering by state, county, NTEE
subsector, and exempt organization type.

The package requires no API keys or authentication — the data is
publicly accessible.

## Exploring the Data Dictionary

Before querying data, you can explore the 97 available columns using
[`nccs_dictionary()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_dictionary.md):

``` r

library(nccsdata)

# See all available columns
nccs_dictionary()

# Find geocoding-related columns
nccs_dictionary("geo")

# Find NTEE classification columns
nccs_dictionary("ntee")
```

## Discovering Filter Values

Use
[`nccs_catalog()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_catalog.md)
to see the valid values for each filter before querying:

``` r

# NTEE v2 subsector codes
nccs_catalog("ntee_subsector")
#> [1] "ART" "EDU" "ENV" "HEL" "HMS" "HOS" "IFA" "MMB" "PSB" "REL" "UNI" "UNU"

# State and territory codes
nccs_catalog("state")

# Exempt organization types (e.g., 501(c)(3), 501(c)(4), etc.)
nccs_catalog("exempt_org_type")
```

## Reading Data

The core function is
[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md),
which reads BMF data from S3 with predicate-pushdown filtering for
efficient reads.

### Filter by State

``` r

# All nonprofits in Pennsylvania
pa <- nccs_read(state = "PA")
nrow(pa)
```

### Filter by County

``` r

# Nonprofits in specific Pennsylvania counties
nepa <- nccs_read(
  state = "PA",
  county = c("Lackawanna County", "Luzerne County", "Wayne County")
)
nrow(nepa)
```

### Filter by NTEE Subsector

``` r

# Arts organizations in Pennsylvania
pa_arts <- nccs_read(state = "PA", ntee_subsector = "ART")
nrow(pa_arts)
```

### Filter by Organization Type

The `org_type` argument bundles the most common 501(c)(3) cuts so you
don’t have to manage `subsection_code` and `foundation_code` by hand:

``` r

# All 501(c)(3) public charities
pc <- nccs_read(org_type = "public_charity")

# California private foundations
ca_pf <- nccs_read(state = "CA", org_type = "private_foundation")

# Everything that is not a 501(c)(3)
non_c3 <- nccs_read(org_type = "non_501c3")
```

Pass `org_type = "501c3"` to keep both public charities and private
foundations, or `"all"` (default) to skip the filter entirely.

## Selecting Columns

The BMF parquet file contains 97 columns and is over 400 MB. By default,
[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
returns a curated subset of commonly needed columns. You can customize
this:

``` r

# Specify exact columns (minimizes download size)
pa_slim <- nccs_read(
  state = "PA",
  columns = c("ein", "org_name_display", "geo_county", "nteev2_subsector")
)
names(pa_slim)

# Get all 97 columns (slower)
pa_full <- nccs_read(state = "PA", columns = "all")
```

## Lazy Evaluation

Set `collect = FALSE` to get a lazy Arrow query instead of a tibble.
This is useful for building custom dplyr chains before collecting:

``` r

library(dplyr)

query <- nccs_read(state = "PA", collect = FALSE)

# Add custom filters and transformations
result <- query |>
  filter(geo_county == "Lackawanna County") |>
  select(ein, org_name_display, nteev2_subsector) |>
  collect()
```

## Column Types

The published BMF parquet stores most columns as `character` (the
upstream pipeline stacks vintages and writes them out as strings). By
default
[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
coerces well-known columns to their natural R types on the collected
tibble:

- `asset_amount`, `income_amount`, `revenue_amount`, `geo_score`,
  `geo_distance` → `numeric`.
- `ruling_date`, `tax_period_ymd` → `Date`.
- The `org_addr_is_*`, `ruling_date_is_missing`, `tax_period_is_missing`
  and `group_exemption_is_member` style flags → `logical` (via
  [`nccs_as_indicator()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_as_indicator.md)).

ID-like codes (`subsection_code`, `classification_code`, ZIPs, NTEE
codes) stay as `character` — they’re identifiers, not numbers. Pass
`coerce = FALSE` to skip all coercion and get every column exactly as
published.

## Local Caching

The master BMF parquet is hundreds of MB.
[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
caches it in `tools::R_user_dir("nccsdata", "cache")` on first use and
reuses the local copy on subsequent calls (in the same session and
across sessions). The cached file is refreshed automatically when it’s
older than `cache_max_age` days (30 by default, matching the upstream
monthly rebuild cadence). Disable with `cache = FALSE`, or force a
refresh:

``` r

nccs_cache_dir()
nccs_cache_clear()
```

## Reading Historical Vintages

[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
reads the rolling geocoded master, which the upstream pipeline rebuilds
every month. For analyses that need a specific dated snapshot (papers,
dashboards, regulatory work), use
[`nccs_vintage_url()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_vintage_url.md)
to get the S3 URI for that month, then read the CSV directly:

``` r

uri  <- nccs_vintage_url("2023_07")
dict <- arrow::read_csv_arrow(
  nccs_vintage_url("2023_07", kind = "dictionary")
)
bmf_2023_07 <- arrow::read_csv_arrow(uri)
```

Vintage snapshots are *not* geocoded — they carry raw Form 990 address
columns instead of the master’s `geo_*` columns. Schemas also drift
across vintages and across the legacy seam (pre-modern vintages live
under a `bmf-legacy` path; pass `legacy = TRUE` to
[`nccs_vintage_url()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_vintage_url.md)).
Always inspect the matching data dictionary before filtering or joining.

## Summarizing

[`nccs_summary()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_summary.md)
produces grouped count summaries:

``` r

pa <- nccs_read(state = "PA")

# Total count
nccs_summary(pa)

# Count by county
nccs_summary(pa, group_by = "geo_county")

# Count by county and NTEE subsector
nccs_summary(pa, group_by = c("geo_county", "nteev2_subsector"))
```

## Saving Results

Write summary results to CSV:

``` r

pa <- nccs_read(
  state = "PA",
  county = c("Lackawanna County", "Luzerne County", "Wayne County")
)

nccs_summary(
  pa,
  group_by = c("geo_county", "nteev2_subsector"),
  output_csv = "nepa_nonprofit_counts.csv"
)
```
