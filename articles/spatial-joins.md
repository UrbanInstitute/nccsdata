# Spatial Joins with TIGER/Line Geographies

## Overview

[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
exposes only the coarsest geographic filters that predicate-pushdown
cheaply against the BMF parquet: `state` and `county`. For anything
finer — congressional districts, Census tracts, block groups, ZCTAs,
metropolitan statistical areas — every BMF row carries a precise
geocoded point (`geo_lat`, `geo_lon`), so a spatial join is both more
accurate and more flexible than filtering on the mailing address.

This vignette shows how to take BMF rows from
[`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md)
and join them to TIGER/Line geographies pulled with the
[`tigris`](https://github.com/walkerke/tigris) package, using
[`sf`](https://r-spatial.github.io/sf/) for the spatial operation. None
of these packages are imported by `nccsdata` — install them yourself
when you need them.

``` r

install.packages(c("tigris", "sf"))
```

## Why not filter on city or ZIP?

The BMF’s city and ZIP fields come from the organization’s *mailing*
address, not its operating location, and ZIP codes are USPS delivery
routes rather than analytical geographies. Joining `geo_lat` / `geo_lon`
to TIGER polygons gives you the actual containing geography and lets you
swap one geography for another (tracts vs. districts vs. places) without
re-querying the BMF.

## Pattern

The recipe is the same regardless of target geography:

1.  Pull BMF rows with
    [`nccs_read()`](https://urbaninstitute.github.io/nccsdata/reference/nccs_read.md).
2.  Convert the lat/lon columns to an `sf` point layer in WGS84
    (EPSG:4326).
3.  Pull the TIGER polygon layer for your target geography.
4.  Reproject to a common CRS and `sf::st_join()`.

``` r

library(nccsdata)
library(dplyr)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)
```

## Example 1 — Congressional districts

Attach the 119th Congress district to every nonprofit in Pennsylvania.

``` r

pa <- nccs_read(state = "PA") |>
  filter(!is.na(geo_lat), !is.na(geo_lon))

pa_sf <- st_as_sf(pa, coords = c("geo_lon", "geo_lat"), crs = 4326)

cd <- congressional_districts(state = "PA", cb = TRUE, year = 2024) |>
  st_transform(4326)

pa_with_cd <- st_join(pa_sf, cd["CD119FP"])
```

`pa_with_cd` is an `sf` object — drop the geometry with
`sf::st_drop_geometry()` if you only need the tabular result.

## Example 2 — Census tracts

Roll nonprofits up to the tract level in Allegheny County, PA.

``` r

ac <- nccs_read(state = "PA", county = "Allegheny County") |>
  filter(!is.na(geo_lat), !is.na(geo_lon)) |>
  st_as_sf(coords = c("geo_lon", "geo_lat"), crs = 4326)

tracts_ac <- tracts(state = "PA", county = "Allegheny", year = 2023) |>
  st_transform(4326)

ac_with_tract <- st_join(ac, tracts_ac["GEOID"])

orgs_per_tract <- ac_with_tract |>
  st_drop_geometry() |>
  count(GEOID, name = "n_orgs") |>
  arrange(desc(n_orgs))
```

## Example 3 — ZCTAs (the right way to use ZIPs)

If your downstream question really is ZIP-based, use Census ZCTAs rather
than the BMF’s mailing ZIP — they’re stable polygons you can join to ACS
and other Census tabulations.

For ZCTAs from 2020 onward, `tigris::zctas()` only serves the full
national layer — the `state` argument was retired because the Census
Bureau stopped publishing state-level ZCTA files. Pull national once (it
caches) and let the spatial join restrict you to California.

``` r

ca <- nccs_read(state = "CA") |>
  filter(!is.na(geo_lat), !is.na(geo_lon)) |>
  st_as_sf(coords = c("geo_lon", "geo_lat"), crs = 4326)

zcta <- zctas(year = 2020) |>
  st_transform(4326)

ca_with_zcta <- st_join(ca, zcta["ZCTA5CE20"])
```

## Notes

- `tigris` caches downloaded shapefiles in the user’s R cache directory
  when `tigris_use_cache = TRUE` — large pulls (every U.S. tract)
  download once and reuse on subsequent calls.
- Choose the matching TIGER vintage when joining against ACS or
  decennial Census data, otherwise GEOIDs may not align across years.
- Points falling outside the TIGER layer (foreign addresses, unmatched
  geocodes) yield `NA` in the joined columns — filter them out before
  tabulation if needed.
