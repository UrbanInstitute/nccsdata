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

# NTEE v2 subsectors. For fields where a bare code is opaque, each value is
# returned as an inline "CODE - Description" string (accepted back verbatim by
# nccs_read()). Pass labels = TRUE for a code + description tibble instead.
nccs_catalog("ntee_subsector")
#>  [1] "ART - Arts, Culture, and Humanities"  "EDU - Education"
#>  [3] "ENV - Environment and Animals"        "HEL - Health"
#>  [5] "HMS - Human Services"                 "IFA - International, Foreign Affairs"
#>  [7] "PSB - Public, Societal Benefit"       "REL - Religion Related"
#>  [9] "MMB - Mutual/Membership Benefit"      "UNU - Unknown, Unclassified"
#> [11] "UNI - Universities"                   "HOS - Hospitals"

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

## Joining External Data on EINs

The `ein` column returned by
[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
is already in canonical `XX-XXXXXXX` form. When you bring your own data
— a CSV export, a spreadsheet, an API response — its EINs are often
messier: numeric storage drops the leading zero, or the value arrives
padded or unformatted. Use
[`nccs_normalize_ein()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_normalize_ein.md)
to coerce arbitrary input to the canonical form before joining:

``` r

# All of these refer to the same organization.
nccs_normalize_ein(c("042104327", "04-2104327", 42104327))
#> [1] "04-2104327" "04-2104327" "04-2104327"
```

Some legacy and NODC products (harmonized CORE marts, the Unified BMF,
the website e-file series) key on `EIN2` — the same EIN with a literal
`EIN-` prefix (`EIN-04-2104327`).
[`nccs_ein_to_ein2()`](https://urbaninstitute.github.io/nccsdata/reference/ein_bridge.md)
and
[`nccs_ein2_to_ein()`](https://urbaninstitute.github.io/nccsdata/reference/ein_bridge.md)
bridge the two formats so you can join a canonical `ein` column against
an `EIN2` key without a crosswalk:

``` r

# Canonical ein -> legacy EIN2 key
nccs_ein_to_ein2("04-2104327")
#> [1] "EIN-04-2104327"

# Legacy EIN2 key -> canonical ein (leading zeros preserved)
nccs_ein2_to_ein("EIN-00-0000004")
#> [1] "00-0000004"
```

Both helpers are vectorized and lenient: an input that cannot be
normalized becomes `NA` and the call warns with a count, so partial loss
never passes silently. They share one normalization core with the
upstream producer, so the round-trip is lossless.

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

## Form 990 Filings (CORE Series)

The BMF is one row per registered organization. To work with one row per
*filing*, use the CORE Series — Form 990 filings harmonized into a
single schema across vintages. Three tiers are published:

- `"merged"` (canonical, default): legacy + SOI-current merged on
  `(ein, tax_period)`. One row per `(ein, tax_period)`, 1987-2024, forms
  `990combined` and `990pf`. **Deduplicated** — keeps the first
  occurrence per key.
- `"soi"`: IRS SOI annual extracts, harmonized. 2012-2024, forms `990`,
  `990ez`, `990pf`, `990combined`. Carries an `is_amendment` flag if you
  need to separate originals from revisions.
- `"legacy"`: NCCS legacy CORE files, harmonized. 1987-2011, forms
  `990combined` and `990pf`.

``` r

# Inspect a partition's columns before reading
dict <- nccs_core_columns("merged", 2020, "990combined")
head(dict[, c("harmonized_name", "data_type", "description")])

# Read one partition with column projection
df <- nccs_read_core(
  tier     = "merged",
  tax_year = 2020,
  form     = "990combined",
  columns  = c("ein", "tax_period", "total_revenue", "total_expenses")
)

# Or build a lazy query for custom filters
nccs_read_core("merged", 2020, "990combined", collect = FALSE) |>
  dplyr::filter(subsection_cd == 3) |>
  dplyr::select(ein, tax_period, total_revenue) |>
  dplyr::collect()
```

For multi-year analyses, pass a vector to `tax_year`. The function
stacks the requested partitions into one Arrow dataset (so filters and
column projection still push down across all of them) and, in an
interactive session, reports the total download size before fetching any
partitions it doesn’t already have cached.

``` r

panel <- nccs_read_core(
  tier     = "merged",
  tax_year = 2015:2022,
  form     = "990combined",
  columns  = c("ein", "tax_period", "total_revenue", "total_expenses")
)
```

[`nccs_core_coverage()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_coverage.md)
returns row counts for every published partition of a tier — useful for
sanity checks and quickly spotting coverage gaps (e.g. SOI’s missing
2017-2019 `990pf` partitions).

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
