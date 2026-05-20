# Read NCCS CORE Series Partitions

Reads one or more \`(tier, tax_year, form)\` Form 990 partitions as
parquet, with optional column projection, dplyr-style filtering, and
local caching. Pass a vector of years to \`tax_year\` to read multiple
partitions in a single query (e.g. \`tax_year = 2015:2022\`); the
partitions are stacked into one Arrow dataset so filters and column
projection push down across all of them.

## Usage

``` r
nccs_read_core(
  tier = c("merged", "soi", "legacy"),
  tax_year,
  form,
  columns = NULL,
  cache = TRUE,
  cache_max_age = 30L,
  collect = TRUE,
  confirm = interactive()
)
```

## Arguments

- tier:

  One of \`"merged"\` (default, canonical), \`"soi"\`, or \`"legacy"\`.
  See Description.

- tax_year:

  Integer tax year, or an integer vector of years for a multi-partition
  read.

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

  Local cache controls. \`TRUE\` (default) caches each parquet under
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

- confirm:

  Logical. If \`TRUE\` and a multi-partition download is required,
  prompt for confirmation after reporting the total size. Defaults to
  \`interactive()\` so scripts and tests proceed silently while
  interactive sessions get a guardrail.

## Value

A tibble (if \`collect = TRUE\`) or an Arrow Dataset query (if \`collect
= FALSE\`).

## Details

One row per filing (or per \`(ein, tax_period)\` in the deduplicated
\`"merged"\` tier).

When the request would download multiple partitions, the function
reports the total transfer size (summed over partitions not already
cached) and, if \`confirm = TRUE\`, prompts before downloading.
Partitions that are not published for the requested tier (e.g. SOI
\`990pf\` for 2017-2019) are dropped with a \`message()\` rather than
raising an error, so a year range that straddles a gap still works.

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

# Multi-year: any partition that turns out to be unpublished
# upstream is dropped with a message rather than erroring.
panel <- nccs_read_core(
  tier     = "soi",
  tax_year = 2015:2022,
  form     = "990pf",
  columns  = c("ein", "tax_period", "total_revenue")
)

# Lazy query, custom filter, then collect
nccs_read_core("merged", 2018:2022, "990combined", collect = FALSE) |>
  dplyr::filter(subsection_cd == 3) |>
  dplyr::select(ein, tax_period, total_revenue) |>
  dplyr::collect()
} # }
```
