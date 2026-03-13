# Grouped Count Summaries

Produces grouped count summaries from a collected BMF data frame. Useful
for generating county-level or subsector-level nonprofit counts.

## Usage

``` r
nccs_summary(data, group_by = NULL, output_csv = NULL)
```

## Arguments

- data:

  A data frame or tibble, typically the output of \[nccs_read()\].

- group_by:

  Character vector of column names to group by before counting. If
  \`NULL\` (default), returns the total row count.

- output_csv:

  Optional file path. If provided, writes the result to a CSV file.

## Value

A tibble with the grouping columns (if any) and a count column \`n\`.

## Examples

``` r
if (FALSE) { # \dontrun{
pa <- nccs_read(state = "PA")

# Total count
nccs_summary(pa)

# Count by county
nccs_summary(pa, group_by = "geo_county")

# Count by county and subsector, save to CSV
nccs_summary(pa, group_by = c("geo_county", "nteev2_subsector"),
             output_csv = "counts.csv")
} # }
```
