# Read an NCCS CORE Series Partition

Reads one \`(tier, tax_year, form)\` Form 990 partition as parquet, with
optional column projection, dplyr-style filtering, and local caching.
One row per filing (or per \`(ein, tax_period)\` in the deduplicated
\`"merged"\` tier).

## Usage

``` r
nccs_read_core(
  tier = c("merged", "soi", "legacy"),
  tax_year,
  form,
  columns = NULL,
  cache = TRUE,
  cache_max_age = 30L,
  collect = TRUE
)
```

## Arguments

- tier:

  One of \`"merged"\` (default, canonical), \`"soi"\`, or \`"legacy"\`.
  See Description.

- tax_year:

  Integer tax year.

- form:

  Character form code: \`"990"\`, \`"990ez"\`, \`"990pf"\`, or
  \`"990combined"\`. Not every form exists in every tier — see
  Description.

- columns:

  Optional character vector of column names to project. \`NULL\`
  (default) returns all columns. Parquet projection means unselected
  columns are never read from disk or wire. Use \[nccs_core_columns()\]
  to see what is available for a partition.

- cache:

  Local cache controls. \`TRUE\` (default) caches the parquet under
  \[nccs_cache_dir()\] in a \`core/\<tier\>/\<tax_year\>/\<form\>/\`
  subdir. A character path uses that directory instead. \`FALSE\` reads
  directly from S3 (slower on repeat calls, lower disk usage).

- cache_max_age:

  Integer. Maximum age in days before the cached parquet is considered
  stale and re-downloaded. Defaults to 30. Ignored when \`cache =
  FALSE\`.

- collect:

  Logical. \`TRUE\` (default) returns a tibble. \`FALSE\` returns a lazy
  Arrow query for further dplyr operations and a final
  \`dplyr::collect()\`.

## Value

A tibble (if \`collect = TRUE\`) or an Arrow Dataset query (if \`collect
= FALSE\`).

## Details

For multi-year queries, prefer building the dataset yourself with
\`arrow::open_dataset()\` over a glob — see Examples. This function
deliberately reads a single partition so caching, column dictionaries,
and schema reasoning stay simple.

See \[nccs_core_url()\] for the canonical URL pattern and a description
of each tier's coverage and caveats (especially the \`"merged"\` tier's
deduplication and the missing 2017-2019 \`990pf\` partitions).

## See also

\[nccs_core_url()\], \[nccs_core_columns()\], \[nccs_core_coverage()\].

## Examples

``` r
if (FALSE) { # \dontrun{
# One partition, a handful of columns
df <- nccs_read_core(
  tier = "merged",
  tax_year = 2020,
  form = "990combined",
  columns = c("ein", "tax_period", "total_revenue", "total_expenses")
)

# Lazy query, custom filter, then collect
nccs_read_core("merged", 2020, "990combined", collect = FALSE) |>
  dplyr::filter(subsection_cd == 3) |>
  dplyr::select(ein, tax_period, total_revenue) |>
  dplyr::collect()

# Multi-year: build the dataset directly
arrow::open_dataset(
  paste0("s3://nccsdata/processed_merged/core/*/990combined/",
         "core_*_990combined.parquet"),
  format = "parquet"
) |>
  dplyr::filter(tax_year >= 2015) |>
  dplyr::collect()
} # }
```
