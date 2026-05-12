# Normalize EINs to Canonical IRS Format

Coerces a vector of Employer Identification Numbers (EINs) to the
canonical IRS display format \`XX-XXXXXXX\` (10 characters, two digits,
hyphen, seven digits). Useful for users joining external data (CSV
exports, spreadsheets, API responses) against the BMF returned by
\[nccs_read()\], whose \`ein\` column is already normalized upstream.

## Usage

``` r
nccs_normalize_ein(x)
```

## Arguments

- x:

  A character or numeric vector of EINs in any format (\`"123456789"\`,
  \`"12-3456789"\`, \`123456789\`, \`"1234567"\`, etc.).

## Value

A character vector the same length as \`x\`, with each element either in
\`XX-XXXXXXX\` form or \`NA_character\_\`.

## Details

The function strips all non-digit characters, left-pads to nine digits
with zeros (to recover EINs that lost a leading zero during numeric
coercion), and inserts the hyphen at position 3. Values that cannot be
reduced to a non-empty digit string of nine or fewer characters become
\`NA_character\_\`.

Pure base-R, no side effects, no in-place mutation.

## Examples

``` r
nccs_normalize_ein(c("123456789", "12-3456789", 123456789))
#> [1] "12-3456789" "12-3456789" "12-3456789"
# All three become "12-3456789".

nccs_normalize_ein("1234567")       # leading zeros restored -> "00-1234567"
#> [1] "00-1234567"
nccs_normalize_ein(c("abc", NA))    # both become NA
#> [1] NA NA
```
