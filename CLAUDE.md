# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## About

`nccsdata` is an R package for downloading, filtering, and analyzing
NCCS (National Center for Charitable Statistics) nonprofit organization
data. Published by the Urban Institute.

It is a lean downstream consumer of artifacts published by the sibling
ETL pipelines. This package does not transform raw IRS data — it reads
parquet that has already been cleaned upstream and bundles a small set
of decoder lookups for convenience.

## Common Commands

``` bash
# Check package (equivalent of build + test + lint)
R CMD check .

# Run all tests
Rscript -e 'devtools::test()'

# Run a single test file
Rscript -e 'testthat::test_file("tests/testthat/test-<name>.R")'

# Regenerate documentation from roxygen2 comments
Rscript -e 'devtools::document()'

# Build pkgdown documentation site
Rscript -e 'pkgdown::build_site()'

# Install package locally
R CMD INSTALL .

# Refresh bundled lookup tables from S3 (dev only, requires sha256sum on PATH)
Rscript data-raw/build_lookups.R
```

## Design constraints

These rules are load-bearing. Violating them tends to bloat the package
and break the upstream/downstream contracts.

- **Lean dependencies.** Hard `Imports` are exactly `arrow`, `dplyr`,
  `utils`. Do **not** add `data.table`, `stringr`, `lubridate`, `log4r`,
  `openxlsx`, or any other heavyweight dep without first asking the
  user. Reference pipelines (`../nccs-data-bmf/`, `../nccs-data-core/`)
  use these freely — this package does not.
- **Pure functions.** Helpers take input and return output; no in-place
  mutation (e.g. `data.table::set`), no global state, no logger handles.
  Diagnostics use base
  [`warning()`](https://rdrr.io/r/base/warning.html) /
  [`message()`](https://rdrr.io/r/base/message.html).
- **Naming.** Exported functions are `nccs_*` (snake_case). Internal
  helpers use a leading dot (e.g. `.build_s3_path`) and `@noRd`.
- **No re-cleaning of package output.** Helpers like
  [`nccs_normalize_ein()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_normalize_ein.md)
  exist for users bringing *external* data; rows returned by
  [`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
  are already normalized upstream and should not be re-run through them.

## Upstream contract

The BMF parquet read by
[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
is published by `../nccs-data-bmf/`. Value-level cleaning is done
upstream, but the parquet’s physical types are mostly `string` — see
“Column types” below.

- `ein` is in canonical `XX-XXXXXXX` form.
- `nteev2_*` columns are decoded (subsector, code, definition, org
  type).
- `subsection_code` is mapped to a verbose `exempt_organization_type`
  description.
- Geocoding columns (`geo_*`) are populated.

This package does not implement tax-period, NTEE, or subsection
transforms — those live in the ETL pipeline. The audit conclusion from
the v2 design sessions was: only EIN normalization and binary-indicator
coercion are worth promoting as user-facing helpers, because users
frequently bring external CSVs into the same workflow.

### Column types

Upstream stacks BMF vintages with mixed source dtypes and writes the
result with almost every column as `string`, by design — explicit typing
failed across the vintage seam. Concretely:

- Numeric-looking columns (`asset_amount`, `income_amount`,
  `revenue_amount`, `geo_score`, `geo_distance`, …) arrive as
  `character`. Cast with
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html) at the consumer.
- Code columns (`subsection_code`, `classification_code`,
  `foundation_code`, `affiliation_code`, `accounting_period`,
  `asset_code`, `income_code`, `filing_requirement_code`,
  `org_addr_zip5`, `org_addr_zip4`, `geo_postal`, `activity_code`,
  `ruling_date_ym_str`, `tax_period_ym_str`, …) arrive as `character`.
  Treat them as IDs; cast only if needed.
- Boolean-looking columns (`group_exemption_is_member`,
  `org_addr_is_po_box`, `org_addr_is_rural_route`,
  `org_addr_has_special_chars`, `org_addr_is_missing`,
  `org_addr_missing_number`, `org_addr_state_invalid`,
  `ruling_date_is_missing`, `tax_period_is_missing`,
  `in_care_of_name_provided`) arrive as `character` (“True”/“False”).
  Use
  [`nccs_as_indicator()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_as_indicator.md)
  to coerce — these are exactly the case the helper was built for, so
  the “no re-cleaning of package output” rule does not apply to
  indicator/financial coercion here.
- Date columns (`ruling_date`, `tax_period_ymd`) arrive as `character`
  in `YYYY-MM-DD` form. Cast with
  [`as.Date()`](https://rdrr.io/r/base/as.Date.html).
- Columns that *do* keep proper types: `geo_lat`/`geo_lon` (double),
  `geo_is_geocoded` (logical), `first_year_in_bmf` / `last_year_in_bmf`
  (integer), `bmf_vintages_observed` (double).

The bundled data dictionary (`bmf_dictionary`) records the actual
on-disk type for each column, so it is the source of truth — not this
list, which is illustrative.

## Architecture

### Data Source

[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
reads the rolling master geocoded BMF parquet at:
`s3://nccsdata/geocoding/bmf-master/merged/bmf_master_geocoded.parquet`

The upstream pipeline also publishes dated monthly snapshots at
`s3://nccsdata/geocoding/bmf/{YYYY_MM}/merged/bmf_{YYYY_MM}_geocoded.parquet`,
but this package does not expose them. Users who need a specific vintage
should call
[`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
against that path directly.

The `arrow` package reads from S3 directly via URI — no authentication
needed.

### Package Functions

- `R/nccs_read.R` — Core function. Reads BMF parquet from S3 with
  optional predicate-pushdown filters (state, county, NTEE subsector,
  exempt org type). Supports column selection and lazy Arrow queries.
- `R/nccs_core_url.R`, `R/nccs_read_core.R`, `R/nccs_core_columns.R`,
  `R/nccs_core_coverage.R` — CORE Series (Form 990 filings) surface.
  Three tiers (`merged` / `soi` / `legacy`) under
  `s3://nccsdata/processed{,_legacy,_merged}/core/{tax_year}/{form}/`.
  Each partition has a parquet + CSV data file and a parquet + CSV
  column dictionary.
  [`nccs_read_core()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read_core.md)
  accepts a vector `tax_year` and stacks the requested partitions into
  one Arrow dataset; before downloading anything to the cache it reports
  total transfer size and, if `confirm = interactive()`, prompts.
  Partitions that don’t exist upstream are dropped with a
  [`message()`](https://rdrr.io/r/base/message.html) rather than
  erroring, so a year range that straddles a gap still works. Tier
  coverage notes (legacy: no `990` / `990ez`; SOI: 2017-2019 `990pf`
  partitions are present but only contain backfilled tax-year rows from
  2020+ extracts — sparse, no original calendar-year publication;
  merged: dedup keeps first by `(ein, tax_period)`) are documented in
  [`nccs_core_url()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_core_url.md)
  and validated by `.validate_core_partition()`.
- `R/nccs_summary.R` — Grouped count summaries on collected data.
- `R/nccs_catalog.R` — Lists valid filter values (offline, no network
  needed). Supports `labels = TRUE` to return a code + description
  tibble for fields backed by a bundled lookup.
- `R/nccs_dictionary.R` — Returns BMF data dictionary as a tibble with
  optional grep filtering. Also documents the `bmf_dictionary` dataset.
- `R/nccs_normalize_ein.R` — Coerces arbitrary EIN inputs to canonical
  `XX-XXXXXXX` form. Pure, base-R only.
- `R/nccs_ein_bridge.R` —
  [`nccs_ein_to_ein2()`](https://urbaninstitute.github.io/nccsdata/reference/ein_bridge.md)
  /
  [`nccs_ein2_to_ein()`](https://urbaninstitute.github.io/nccsdata/reference/ein_bridge.md)
  bridge the canonical clean `ein` (`XX-XXXXXXX`) and the legacy/NODC
  `EIN2` (`EIN-XX-XXXXXXX`) join keys. Deterministic string reformat (no
  lookup); both route through the canonical 9-digit-`core()`
  normalization. Implements the `nccs-contracts` convention
  `conventions/ein-format.md` (consumer side; producer is
  `nccs-data-bmf/R/ein.R::transform_ein`). Lenient contract:
  un-normalizable input → `NA` plus a
  [`warning()`](https://rdrr.io/r/base/warning.html) with the drop
  count. Pure, base-R only.
- `R/nccs_as_indicator.R` — Coerces IRS binary-indicator columns to
  logical, with `yn` and `efile` schemes for vintage-specific encodings.
- `R/nccs_deflate.R` + `R/cpi_u.R` — Real-dollar conversion for CORE
  financial columns. `nccs_deflate(amount, year, base_year)` uses the
  bundled annual CPI-U series (FRED `CPIAUCNS`, 1913-present, refreshed
  via `data-raw/build_cpi.R`). This is the *one* opinionated analytic
  helper in the package — canonical financial ratios (operating margin,
  program-expense ratio, etc.) are deliberately not bundled because
  their definitions vary by analyst.

### Package Data

- `data/bmf_dictionary.rda` — 106-row tibble with column_name,
  description, type for all BMF columns. Exported.
- `data/cpi_u.rda` — Annual CPI-U series (1913-present) backing
  [`nccs_deflate()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_deflate.md).
  Exported. Refreshed by `data-raw/build_cpi.R`.
- `R/sysdata.rda` — Internal bundled lookup tables (see “Bundled
  lookups” below). Not exported; consumed by
  `nccs_catalog(labels = TRUE)`.
- `data-raw/bmf_data_dictionary.csv` — Source CSV for `bmf_dictionary`.
- `data-raw/data_generation.R` — Script to regenerate
  `bmf_dictionary.rda`.
- `data-raw/build_lookups.R` — Script to refresh `R/sysdata.rda` from
  S3.

### Structure

- `R/` — Function files
- `man/` — Generated by roxygen2
- `tests/testthat/` — Tests for each function
- `vignettes/getting-started.Rmd` — Introductory vignette (eval=FALSE to
  avoid S3 calls during build)
- `_pkgdown.yml` — Site config with function references
- `hex/` — Branding assets
- `.github/` — CI workflows

### Dependencies

- **Imports**: `arrow`, `dplyr`, `utils`
- **Suggests**: `curl`, `knitr`, `rmarkdown`, `testthat`

## Bundled lookups

A curated subset of the BMF decoder tables is bundled as internal
package data so that `nccs_catalog(field, labels = TRUE)` works offline.

- **Source**: `s3://nccsdata/lookups/bmf/latest/{lookup_name}.csv` plus
  `MANIFEST.json` (sha256 per file, vintage, generated_at). Published by
  `../nccs-data-bmf/` — see that repo’s CLAUDE.md § “Published
  artifacts”.
- **Curated subset** (selected because they decode codes users see in
  external data they bring to the package):
  - `subsection_classification_code` — subsection → 501(c)(X)
    description
  - `nteev2_subsector` — NTEE v2 subsector → definition
  - `foundation_code` — foundation type
  - `affiliation_code` — affiliation type
- **Not bundled** (large or pipeline-internal): `ntee_code`,
  `ntee_common_code`, `activity_code`, the filing-requirement tables,
  `ntee_legacy_5char`, etc. Ask before adding any of these.
- **Storage**: `R/sysdata.rda` contains a single list `.nccs_lookups`
  with one named element per bundled table, plus a `.metadata` element
  holding the manifest’s `vintage`, `generated_at`, and per-file
  `sha256`. The package can self-report its lookup vintage via
  `.nccs_lookups$.metadata`.
- **Refresh**: run `Rscript data-raw/build_lookups.R` from the package
  root. The script downloads CSVs + manifest, validates sha256 against
  the manifest (using system `sha256sum`), prints a diff summary against
  the previously bundled version, and writes `R/sysdata.rda`. It is
  idempotent — re-running with no upstream change reproduces the same
  `.rda`.

## Related repos

Sibling repos under `../` (S3 is the contract surface — this package
must not import from them directly):

- `../nccs-data-bmf/` — BMF ETL pipeline. Source of truth for the
  geocoded BMF parquet and the published lookup tables this package
  consumes. See its `CLAUDE.md` § “Published artifacts” for the S3
  contract.
- `../nccs-data-core/` — Form 990 ETL pipeline. Not currently consumed
  by this package; transforms there (EIN, indicators, e-file) served as
  reference implementations for
  [`nccs_normalize_ein()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_normalize_ein.md)
  and
  [`nccs_as_indicator()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_as_indicator.md).

## Testing

Tests use testthat edition 3. Network-dependent integration tests use
`skip_on_cran()` and `skip_if_offline()`.

- `test-nccs_dictionary.R` — Dictionary dataset structure and filtering
  (offline)
- `test-nccs_catalog.R` — Valid filter values, label decoding, error
  handling (offline)
- `test-nccs_summary.R` — Count summaries, grouping, CSV output
  (offline)
- `test-nccs_read.R` — S3 path construction, input validation (offline),
  integration reads (network)
- `test-nccs_normalize_ein.R` — EIN coercion happy/edge paths (offline)
- `test-nccs_as_indicator.R` — yn/efile coercion, unknown-value warning
  (offline)

## Contract-change guard (ADR 0022)

A PR that changes how this package **reads or pins** contracted
artifacts — the read/url/vintage/catalog/column code — must acknowledge
the [`nccs-contracts`](https://github.com/UrbanInstitute/nccs-contracts)
impact, or CI fails. The `.github/workflows/contracts-guard.yml` caller
(a thin wrapper over the reusable guard in `nccs-contracts`) fires on
PRs that change `R/nccs_read*.R`, `R/nccs_vintage_url.R`,
`R/nccs_core_url.R`, `R/nccs_catalog.R`, or
`R/nccs_core_columns.R`/`R/nccs_core_coverage.R`. To pass: add an
`ADR NNNN` breadcrumb to a commit message or the PR body (and queue the
`nccs-contracts` reconcile if a contract pin/version moved), **or** add
the `contracts-ack` label if there’s genuinely no contract impact. The
guard checks *acknowledgment, not correctness*.
