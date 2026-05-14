# Manage the Local BMF Cache

\`nccs_read()\` downloads the geocoded master BMF parquet (hundreds of
MB) from S3 on first use and reuses a local copy on subsequent calls.
These helpers expose the cache directory used by default and let you
wipe it.

## Usage

``` r
nccs_cache_dir()

nccs_cache_clear(dir = nccs_cache_dir())
```

## Arguments

- dir:

  Directory to clear. Defaults to \[nccs_cache_dir()\].

## Value

\`nccs_cache_dir()\` returns the cache directory as a character string.
\`nccs_cache_clear()\` returns the number of files removed, invisibly.

## Details

The default location is \`tools::R_user_dir("nccsdata", "cache")\`. The
cached file is considered stale after \`cache_max_age\` days (30 by
default; the upstream master is rebuilt monthly).

## Examples

``` r
nccs_cache_dir()
#> [1] "/home/runner/.cache/R/nccsdata"

if (FALSE) { # \dontrun{
# Force a refresh on the next nccs_read() call
nccs_cache_clear()
} # }
```
