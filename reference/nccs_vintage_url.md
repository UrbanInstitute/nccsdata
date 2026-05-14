# S3 URL of a Historical BMF Vintage

Returns the S3 URI for a specific dated monthly BMF snapshot or its
per-vintage data dictionary. These artifacts are CSVs with per-vintage
schemas (no central crosswalk) and are \*not\* geocoded — they carry raw
Form 990 address columns instead of the master's \`geo\_\*\` columns.
Use this helper to fetch a URI, then read the file yourself with
\`arrow::read_csv_arrow()\` or \`utils::read.csv()\`.

## Usage

``` r
nccs_vintage_url(vintage, kind = c("data", "dictionary"), legacy = FALSE)
```

## Arguments

- vintage:

  Character string of the form \`"YYYY_MM"\` (e.g., \`"2023_07"\`).

- kind:

  One of \`"data"\` (default) for the BMF snapshot CSV or
  \`"dictionary"\` for the column data dictionary CSV.

- legacy:

  Logical. If \`TRUE\`, returns the legacy-archive path used for older
  vintages (\`processed/bmf-legacy/...\`). Defaults to \`FALSE\` (the
  modern \`processed/bmf/...\` path).

## Value

A single character string with the \`s3://\` URI.

## Details

For analyses that don't require a specific vintage, prefer
\[nccs_read()\], which reads the rolling geocoded master parquet with
Arrow predicate-pushdown filters.

## Examples

``` r
nccs_vintage_url("2023_07")
#> [1] "s3://nccsdata/processed/bmf/2023_07/bmf_2023_07_processed.csv"
nccs_vintage_url("2023_09", kind = "dictionary")
#> [1] "s3://nccsdata/processed/bmf/2023_09/bmf_2023_09_data_dictionary.csv"
nccs_vintage_url("1999_12", legacy = TRUE)
#> [1] "s3://nccsdata/processed/bmf-legacy/1999_12/bmf_legacy_1999_12_processed.csv"

if (FALSE) { # \dontrun{
# Read a vintage snapshot directly. Schemas differ across vintages
# and across the legacy/modern seam — inspect the matching data
# dictionary before filtering or joining.
uri <- nccs_vintage_url("2023_07")
bmf_2023_07 <- arrow::read_csv_arrow(uri)
} # }
```
