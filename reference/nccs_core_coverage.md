# Row-Count Coverage of a CORE Series Tier

Returns a tibble of \`(tax_year, form, n_rows, url)\` for every
published partition of a tier. Row counts are read from each parquet's
footer metadata (no full file scan), but one HTTP request per partition
is still issued, so this typically takes 10-60s depending on tier.

## Usage

``` r
nccs_core_coverage(tier = c("merged", "soi", "legacy"))
```

## Arguments

- tier:

  One of \`"merged"\` (default), \`"soi"\`, or \`"legacy"\`.

## Value

A tibble with columns \`tax_year\` (integer), \`form\` (character),
\`n_rows\` (double), and \`url\` (character HTTPS URL of the parquet).

## Details

Missing partitions (e.g. SOI-current \`990pf\` for 2017-2019) are
omitted from the result rather than reported as zero rows. The canonical
coverage tables are described in \[nccs_core_url()\].

## See also

\[nccs_core_url()\], \[nccs_read_core()\].

## Examples

``` r
if (FALSE) { # \dontrun{
# Coverage of the canonical merged tier
cov <- nccs_core_coverage("merged")
dplyr::summarise(cov, total = sum(n_rows), .by = form)
} # }
```
