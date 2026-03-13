# Read NCCS BMF Data from S3

Reads the NCCS Business Master File (BMF) stored as a parquet file in a
public S3 bucket. Supports predicate-pushdown filtering by state,
county, NTEE subsector, and exempt organization type for efficient
reads.

## Usage

``` r
nccs_read(
  state = NULL,
  ntee_subsector = NULL,
  exempt_org_type = NULL,
  county = NULL,
  columns = NULL,
  date = "2026_03",
  collect = TRUE
)
```

## Arguments

- state:

  Character vector of two-letter state abbreviations to filter on (e.g.,
  \`"PA"\`, \`c("PA", "NY")\`). Filters the \`org_addr_state\` column.

- ntee_subsector:

  Character vector of NTEE v2 subsector codes (e.g., \`"ART"\`,
  \`c("ART", "EDU")\`). See \[nccs_catalog()\] for valid values. Filters
  the \`nteev2_subsector\` column.

- exempt_org_type:

  Character vector of exempt organization type descriptions. See
  \[nccs_catalog()\] for valid values. Filters the
  \`exempt_organization_type\` column.

- county:

  Character vector of county names (e.g., \`"Lackawanna County"\`).
  Filters the \`geo_county\` column.

- columns:

  Column selection. \`NULL\` (default) returns a sensible default
  subset. A character vector returns those specific columns. \`"all"\`
  returns all columns (warning: 400+ MB). Columns used in active filters
  are always included.

- date:

  Character string in \`YYYY_MM\` format indicating the data vintage.
  Defaults to \`"2026_03"\`.

- collect:

  Logical. If \`TRUE\` (default), collects the result into a tibble. If
  \`FALSE\`, returns a lazy Arrow query for further dplyr operations.

## Value

A tibble (if \`collect = TRUE\`) or an Arrow Dataset query (if \`collect
= FALSE\`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Read all Pennsylvania nonprofits
pa <- nccs_read(state = "PA")

# Read with specific columns
pa <- nccs_read(state = "PA", columns = c("ein", "org_name_display", "geo_county"))

# Lazy query for custom dplyr chains
query <- nccs_read(state = "PA", collect = FALSE)
result <- query |> dplyr::filter(geo_county == "Lackawanna County") |> dplyr::collect()
} # }
```
