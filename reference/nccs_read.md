# Read NCCS BMF Data from S3

Reads the NCCS Business Master File (BMF) stored as a parquet file in a
public S3 bucket. Supports predicate-pushdown filtering on state,
county, NTEE classification (subsector, code, NTEEv2 code, major group),
exempt organization type, financial size, and BMF recency for efficient
reads.

## Usage

``` r
nccs_read(
  state = NULL,
  county = NULL,
  ntee_subsector = NULL,
  ntee_major_group = NULL,
  ntee_code = NULL,
  nteev2_code = NULL,
  exempt_org_type = NULL,
  size_metric = c("revenue", "income", "asset"),
  size_min = NULL,
  size_max = NULL,
  min_last_year = NULL,
  columns = NULL,
  collect = TRUE
)
```

## Arguments

- state:

  Character vector of two-letter state abbreviations (e.g., \`"PA"\`,
  \`c("PA", "NY")\`). Filters \`org_addr_state\`.

- county:

  Character vector of county names (e.g., \`"Lackawanna County"\`).
  Filters \`geo_county\`.

- ntee_subsector:

  Character vector of NTEEv2 subsector values. Accepts either subsector
  codes (\`"UNI"\`, \`"ART"\`) or human-readable names
  (\`"Universities"\`, \`"Arts, Culture and Humanities"\`), matched
  case-insensitively. See \[nccs_catalog()\] for valid values. Filters
  \`nteev2_subsector\`.

- ntee_major_group:

  Character vector of single-letter NTEE major groups (\`"A"\` through
  \`"Z"\`). Filters \`ntee_code_major_group\`. See
  \[nccs_catalog("ntee_major_group", labels = TRUE)\] for descriptions.

- ntee_code:

  Character vector of standardized 3-character NTEE codes (e.g.,
  \`"B40"\`, \`c("A20", "A23")\`). Filters \`ntee_code_clean\`. Not
  validated — invalid codes just return no rows.

- nteev2_code:

  Character vector of NTEEv2 3-character codes. Filters \`nteev2_code\`.
  Not validated.

- exempt_org_type:

  Character vector of exempt organization type descriptions. See
  \[nccs_catalog()\] for valid values. Filters
  \`exempt_organization_type\`.

- size_metric:

  One of \`"revenue"\`, \`"income"\`, or \`"asset"\` indicating which
  financial amount to use with \`size_min\` / \`size_max\`. Defaults to
  \`"revenue"\`. The underlying columns are stored upstream as
  character; this function casts to numeric inside the predicate so the
  filter pushes down to Arrow.

- size_min, size_max:

  Numeric. Optional inclusive bounds on the \`size_metric\` amount.
  \`NULL\` (default) leaves a side unbounded. Rows with \`NA\` for the
  chosen metric are dropped when either bound is set.

- min_last_year:

  Integer. If set, restricts results to EINs whose \`last_year_in_bmf\`
  (the calendar year of the most recent BMF vintage in which the EIN
  appeared) is at least this value. Use this as a recency / "still
  active" filter — e.g., \`min_last_year = 2024\` keeps organizations
  seen in BMF in 2024 or later.

- columns:

  Column selection. \`NULL\` (default) returns a sensible default
  subset. A character vector returns those specific columns. \`"all"\`
  returns all columns (warning: 400+ MB). Columns used in active filters
  are always included.

- collect:

  Logical. If \`TRUE\` (default), collects the result into a tibble. If
  \`FALSE\`, returns a lazy Arrow query for further dplyr operations.

## Value

A tibble (if \`collect = TRUE\`) or an Arrow Dataset query (if \`collect
= FALSE\`).

## Details

The package reads the rolling "master" geocoded BMF published at
\`s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet\`.
The upstream pipeline (\`../nccs-data-bmf/\`) also publishes dated
monthly snapshots at \`s3://nccsdata/geocoding/bmf/YYYY_MM/...\` but
this package does not expose them — point \`arrow::open_dataset()\` at
that path directly if you need a specific vintage.

## Examples

``` r
if (FALSE) { # \dontrun{
# All Pennsylvania nonprofits
pa <- nccs_read(state = "PA")

# Universities in PA seen in BMF in 2024 or later
pa_uni <- nccs_read(
  state = "PA",
  ntee_subsector = "Universities",
  min_last_year = 2024
)

# Arts orgs (major group A) with revenue between $1M and $10M
arts_mid <- nccs_read(
  ntee_major_group = "A",
  size_metric = "revenue",
  size_min = 1e6,
  size_max = 1e7
)

# Lazy query for custom dplyr chains
query <- nccs_read(state = "PA", collect = FALSE)
result <- query |>
  dplyr::filter(geo_county == "Lackawanna County") |>
  dplyr::collect()
} # }
```
