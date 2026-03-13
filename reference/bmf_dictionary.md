# BMF Data Dictionary

A tibble containing column names, descriptions, and types for the NCCS
Business Master File (BMF) parquet dataset.

## Usage

``` r
bmf_dictionary
```

## Format

A tibble with 97 rows and 3 columns:

- column_name:

  Column name in the BMF parquet file

- description:

  Human-readable description of the column

- type:

  Data type (character, integer, numeric, logical, IDate)

## Source

NCCS S3 bucket data dictionary
