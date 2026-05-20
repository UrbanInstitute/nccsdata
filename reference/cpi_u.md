# Annual CPI-U Series

Annual averages of the U.S. Bureau of Labor Statistics Consumer Price
Index for All Urban Consumers (CPI-U), U.S. city average, all items, not
seasonally adjusted. The 1982-84 base period equals 100. Bundled to
support \[nccs_deflate()\] without a network round-trip.

## Usage

``` r
cpi_u
```

## Format

A tibble with two columns:

- \`year\`:

  integer calendar year.

- \`cpi\`:

  numeric annual-average CPI-U.

The \`fred_series\` and \`fetched_on\` attributes record the FRED series
id and the snapshot date.

## Source

U.S. Bureau of Labor Statistics via FRED series
\[CPIAUCNS\](https://fred.stlouisfed.org/series/CPIAUCNS).

## Details

Refresh by running \`Rscript data-raw/build_cpi.R\` from the package
root, which pulls the latest annual aggregation of FRED series
\`CPIAUCNS\` and rewrites \`data/cpi_u.rda\`. BLS publishes the
January-following-the-year annual average each year, so the most recent
year may be a partial average until BLS finalizes it.
