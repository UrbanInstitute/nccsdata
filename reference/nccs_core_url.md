# URL of an NCCS CORE Series Partition

Returns the public HTTPS URL of a Form 990 CORE Series partition (or its
column dictionary) on S3. CORE Series files are one row per filing,
partitioned by \`(tax_year, form)\`. Three tiers are published:

## Usage

``` r
nccs_core_url(
  tier = c("merged", "soi", "legacy"),
  tax_year,
  form,
  format = c("parquet", "csv"),
  kind = c("data", "dictionary")
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

- format:

  One of \`"parquet"\` (default) or \`"csv"\`.

- kind:

  One of \`"data"\` (default) for the filings table or \`"dictionary"\`
  for the per-partition column dictionary.

## Value

A single character HTTPS URL.

## Details

- \`"merged"\` (default, canonical):

  Legacy + SOI-current merged on \`(ein, tax_period)\` with SOI
  precedence. One row per \`(ein, tax_period)\`. Adds
  \`source_pipeline\` and \`has_legacy_augment\` columns. Tax years
  1987-2024. Forms \`990combined\`, \`990pf\`. \*\*Deduplicated\*\* —
  keeps first occurrence per \`(ein, tax_period)\`; use \`"soi"\` or
  \`"legacy"\` instead if you need every original filing plus amendment.

- \`"soi"\`:

  IRS SOI annual extracts, harmonized. Tax years 2012-2024. Forms
  \`990\`, \`990ez\`, \`990pf\`, \`990combined\`. Includes
  \`is_amendment\` for filtering originals vs revisions. 2024 is a
  partial year. \`990pf\` has no 2017-2019 partitions (IRS did not
  publish those PF extracts).

- \`"legacy"\`:

  NCCS legacy CORE files, harmonized. Tax years 1987-2011. Forms
  \`990combined\`, \`990pf\`. No amendment flag — duplicates by \`(ein,
  tax_period)\` exist but are not distinguished. \`1993/990pf\` has only
  ~11k rows.

Each partition contains a data file and a column dictionary, each
published as both parquet and CSV. Prefer parquet — it supports
predicate pushdown and column projection (see \[nccs_read_core()\]).

## See also

\[nccs_read_core()\] for reading the parquet directly,
\[nccs_core_columns()\] for the column dictionary,
\[nccs_core_coverage()\] for a tier's row counts.

## Examples

``` r
nccs_core_url("merged", 2020, "990combined")
#> [1] "https://nccsdata.s3.amazonaws.com/processed_merged/core/2020/990combined/core_2020_990combined.parquet"
nccs_core_url("soi", 2020, "990", kind = "dictionary")
#> [1] "https://nccsdata.s3.amazonaws.com/processed/core/2020/990/core_2020_990_dictionary.parquet"
nccs_core_url("legacy", 1995, "990combined", format = "csv")
#> [1] "https://nccsdata.s3.amazonaws.com/processed_legacy/core/1995/990combined/core_1995_990combined.csv"
```
