# Working with the CORE Series (Form 990 filings)

## Overview

The NCCS **CORE Series** is a set of harmonized Form 990 filings,
partitioned by `(tax_year, form)` and published as parquet on S3. Each
row is one filing — financial totals, organization identifiers, NTEE
codes, exemption status, and so on — already cleaned and column-aligned
across vintages by the upstream pipeline.

Background and column-level documentation live on the NCCS site:

- [CORE datasets overview](https://nccs.urban.org/nccs/datasets/core/)
- [CORE data
  catalog](https://nccs.urban.org/nccs/catalogs/catalog-core.html)

This package exposes four CORE helpers —
[`nccs_core_url()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_url.md),
[`nccs_read_core()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read_core.md),
[`nccs_core_columns()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_columns.md),
[`nccs_core_coverage()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_coverage.md)
— and this vignette shows the workflows they’re designed for.

## Picking a tier

CORE is published in three tiers, each a different harmonization of the
underlying IRS source data:

| Tier | Years | Forms | Row grain | When to reach for it |
|----|----|----|----|----|
| `merged` | 1987-2024 | `990combined`, `990pf` | One row per `(ein, tax_period)` | Default. Deduplicated and provenance-tagged. Best for cross-year analytics. |
| `soi` | 2012-2024 | `990`, `990ez`, `990pf`, `990combined` | One row per filing (incl. amendments) | You need the e-file 990/990EZ form split, or want originals + amendments distinguished. |
| `legacy` | 1987-2011 | `990combined`, `990pf` | One row per filing | Pre-2012 history only. Will become a subset of `merged` over time. |

A rough decision tree:

- **Cross-year panel, current era** → `merged`.
- **Need to separate originals from amendments, or need `990` / `990ez`
  split** → `soi`.
- **Pre-2012 only** → `legacy` (or `merged`, which now includes those
  years).

The `merged` tier carries two extra columns — `source_pipeline` and
`has_legacy_augment` — that let you trace any row back to its upstream
source if you need to.

A caveat worth knowing: SOI `990pf` for 2017-2019 is *technically*
present, but only contains backfilled tax-year rows from the 2020+
calendar-year extracts (no original calendar-year publication exists).
Row counts there are tiny (≈665, 3.3k, 100k). The `merged` tier folds
these in cleanly.

## Inspecting a partition before reading

Every partition publishes its own column dictionary. Use
[`nccs_core_columns()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_columns.md)
to see what’s available without downloading the filings parquet itself.

``` r

library(nccsdata)
library(dplyr)

dict <- nccs_core_columns("merged", 2020, "990combined")
head(dict[, c("harmonized_name", "data_type", "description")])
```

[`nccs_core_url()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_url.md)
returns the canonical S3 URL for any partition or dictionary file if you
want to inspect it outside R:

``` r

nccs_core_url("merged", 2020, "990combined")
nccs_core_url("soi", 2020, "990", kind = "dictionary")
```

## Reading one partition

[`nccs_read_core()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read_core.md)
reads a single `(tier, tax_year, form)` partition with column projection
(only requested columns leave S3) and local caching by default.

``` r

df <- nccs_read_core(
  tier     = "merged",
  tax_year = 2020,
  form     = "990combined",
  columns  = c("ein", "tax_period", "total_revenue",
               "total_expenses", "total_assets_eoy")
)
```

For larger queries, drop `collect = FALSE` to keep the result as an
Arrow query and push filters / aggregations down to the parquet:

``` r

nccs_read_core("merged", 2020, "990combined", collect = FALSE) |>
  filter(subsection_cd == 3, total_revenue > 1e6) |>
  select(ein, tax_period, total_revenue, total_expenses) |>
  collect()
```

## Multi-year panels

Pass a vector to `tax_year`. The function stacks the requested
partitions into one Arrow dataset, so filters and column projection
still push down across all of them. Before downloading anything it isn’t
already caching, the function reports the total transfer size and (in
interactive sessions) prompts:

``` r

panel <- nccs_read_core(
  tier     = "merged",
  tax_year = 2015:2022,
  form     = "990combined",
  columns  = c("ein", "tax_period", "total_revenue",
               "total_expenses", "total_assets_eoy")
)
#> nccs_read_core: downloading 8 partition(s) (~3.4 GB) to local
#> cache for years 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022.
#> Continue? (Yes/no/cancel)
```

Subsequent calls with the same `(tier, form, year)` partition hit the
local cache instead of S3. The cache lives under
[`nccs_cache_dir()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_cache_dir.md)
— see
[`?nccs_cache_clear`](https://urbaninstitute.github.io/nccsdata/reference/nccs_cache_dir.md)
if you need to wipe it.

[`nccs_core_coverage()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_coverage.md)
lists every published partition for a tier with row counts, which is
useful both as a sanity check and for spotting the small / backfilled
partitions:

``` r

nccs_core_coverage("soi") |>
  filter(form == "990pf") |>
  arrange(tax_year)
```

## Joining CORE filings to BMF metadata

CORE rows include the financial fields and form-specific data, but the
richer organizational metadata (NTEE codes, geocoded location,
subsection classifications) lives in the BMF. The two surfaces share
`ein` as a canonical key.

``` r

# Pull 2022 filings from the merged CORE
filings_2022 <- nccs_read_core(
  tier     = "merged",
  tax_year = 2022,
  form     = "990combined",
  columns  = c("ein", "tax_period", "total_revenue", "total_expenses")
)

# Pull BMF rows for California arts orgs only — geo + NTEE come from BMF
ca_arts <- nccs_read(
  state           = "CA",
  ntee_subsector  = "ART"
) |>
  select(ein, organization_name, geo_county, nteev2_code,
         exempt_organization_type)

# EIN is canonical XX-XXXXXXX in both surfaces — direct join
ca_arts_2022 <- inner_join(ca_arts, filings_2022, by = "ein")
```

Bringing your own list of EINs from an external CSV? Run them through
[`nccs_normalize_ein()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_normalize_ein.md)
first so they match the canonical form used by both CORE and BMF:

``` r

external <- read.csv("my_orgs.csv")
external$ein <- nccs_normalize_ein(external$ein)

filings <- inner_join(filings_2022, external, by = "ein")
```

## Notes

- Most columns in CORE parquet land as `character` to keep schemas
  stable across vintages — cast numeric and date columns at the consumer
  ([`as.numeric()`](https://rdrr.io/r/base/numeric.html),
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html)). Use
  [`nccs_as_indicator()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_as_indicator.md)
  for the IRS Yes/No fields.
- [`nccs_core_coverage()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_coverage.md)
  issues one HTTP request per partition (footer metadata only, no full
  scan) and typically takes 10-60 seconds per tier.
- The CORE catalog at
  [nccs.urban.org/nccs/catalogs/catalog-core.html](https://nccs.urban.org/nccs/catalogs/catalog-core.html)
  is the canonical browseable reference for column definitions across
  years and forms.
