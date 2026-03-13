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
