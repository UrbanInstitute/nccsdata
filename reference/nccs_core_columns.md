# Column Dictionary for a CORE Series Partition

Reads the per-partition column dictionary that accompanies every
published CORE Series file. One row per column in the matching data
file, with the harmonized name, source variable + location, dtype,
null/non-null counts, and the vintages where that column was populated.

## Usage

``` r
nccs_core_columns(
  tier = c("merged", "soi", "legacy"),
  tax_year,
  form,
  pattern = NULL
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

- pattern:

  Optional case-insensitive regex applied to \`harmonized_name\` and
  \`description\`. \`NULL\` (default) returns the full dictionary.

## Value

A tibble with one row per column in the partition.

## Details

Use this as the source of truth for "what columns exist in this
partition." Column lists evolve across vintages, so do not hardcode.

## See also

\[nccs_core_url()\], \[nccs_read_core()\].

## Examples

``` r
if (FALSE) { # \dontrun{
# Full dictionary for the 2020 merged 990combined partition
nccs_core_columns("merged", 2020, "990combined")

# Filter to revenue-related columns
nccs_core_columns("merged", 2020, "990combined", pattern = "revenue")
} # }
```
