# Coerce IRS Binary-Indicator Columns to Logical

IRS Business Master File extracts encode binary fields using
inconsistent tokens that vary by vintage. This helper coerces such a
column to a logical vector under one of two well-known schemes.

## Usage

``` r
nccs_as_indicator(x, scheme = c("yn", "efile"))
```

## Arguments

- x:

  A character, numeric, or logical vector.

- scheme:

  One of \`"yn"\` (default) or \`"efile"\`.

## Value

A logical vector the same length as \`x\`.

## Schemes

\`"yn"\` — general yes/no indicator columns (often the \`\_cd\` suffix
in upstream data).

- TRUE: \`Y\`, \`y\`, \`1\`, \`T\`, \`TRUE\`, \`true\`, \`True\`

- FALSE: \`N\`, \`n\`, \`0\`, \`2\`, \`F\`, \`FALSE\`, \`false\`,
  \`False\`

Note that \`"2"\` maps to FALSE: the IRS shifted some binary fields to a
1 (= yes) / 2 (= no) encoding in recent vintages (e.g. some 2022/2023
990 columns).

\`"efile"\` — the IRS e-file indicator, whose accepted tokens vary by
filing year:

- 2015 990 / 990-EZ used \`E\` (electronic) / \`P\` (paper).

- 2016 and 2017 990 / 990-EZ switched to \`Y\` / \`N\`.

- 2018 onwards returned to \`E\` / \`P\`.

All three encodings are accepted:

- TRUE: \`E\`, \`e\`, \`Y\`, \`y\`, \`1\`, \`T\`, \`TRUE\`, \`true\`,
  \`True\`

- FALSE: \`P\`, \`p\`, \`N\`, \`n\`, \`0\`, \`F\`, \`FALSE\`, \`false\`,
  \`False\`

Tokens outside the accepted set become \`NA\`, with a single
\`warning()\` that lists the distinct unknown values (not one per
element). \`NA\` input is propagated silently.

Pure base-R, no side effects, no in-place mutation. Designed for use on
external data the user is joining against \[nccs_read()\] output —
columns returned by \`nccs_read()\` are already cleaned upstream.

## Examples

``` r
nccs_as_indicator(c("Y", "N", "1", "2", NA))
#> [1]  TRUE FALSE  TRUE FALSE    NA
# [1]  TRUE FALSE  TRUE FALSE    NA

nccs_as_indicator(c("E", "P", "Y", "N"), scheme = "efile")
#> [1]  TRUE FALSE  TRUE FALSE
# [1]  TRUE FALSE  TRUE FALSE
```
