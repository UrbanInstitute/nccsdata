# Convert Nominal Dollars to Real (Inflation-Adjusted) Dollars

Adjusts nominal dollar amounts to real dollars expressed in a base year,
using the bundled annual CPI-U series (\[cpi_u\]). The conversion is the
standard \`nominal \* (CPI_base / CPI_year)\`.

## Usage

``` r
nccs_deflate(amount, year, base_year, cpi = NULL)
```

## Arguments

- amount:

  Numeric vector of nominal dollar amounts.

- year:

  Integer vector of years the amounts are denominated in. Length 1 or
  \`length(amount)\`.

- base_year:

  Single integer. The year whose dollars the result should be expressed
  in. Must be present in the bundled series.

- cpi:

  Optional override. A two-column data frame with \`year\` (integer) and
  \`cpi\` (numeric); defaults to the bundled \[cpi_u\] table. Useful if
  you want to use a different deflator (e.g. PCE, GDP) or a custom
  snapshot of CPI-U.

## Value

Numeric vector the same length as \`amount\`, in \`base_year\` dollars.
\`NA\` for any input element whose year is not in the series; a single
warning lists the missing years.

## Details

For CORE Form 990 panels covering many years, deflating financial
columns is usually a prerequisite for any cross-year comparison. This
helper is a thin wrapper around vector arithmetic — it exists to (a)
bundle the CPI lookup table so users don't have to fetch it themselves,
and (b) standardize the lookup behavior (warning on out-of-series years,
NA propagation).

## See also

\[cpi_u\] for the bundled series.

## Examples

``` r
# $100 in 1990 expressed in 2020 dollars
nccs_deflate(100, year = 1990, base_year = 2020)
#> [1] 198.0828

# Vectorized over a column
df <- data.frame(year = c(2010, 2015, 2020),
                 revenue = c(1e5, 1.2e5, 1.4e5))
df$revenue_real_2023 <- nccs_deflate(df$revenue, df$year, base_year = 2023)
df
#>   year revenue revenue_real_2023
#> 1 2010  100000          139735.7
#> 2 2015  120000          154268.4
#> 3 2020  140000          164824.1
```
