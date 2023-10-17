
# nccsdata

<!-- badges: start -->

[![R-CMD-check](https://github.com/UrbanInstitute/nccsdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/UrbanInstitute/nccsdata/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

nccsdata provides tools to read, filter and append metadata to publicly
available NCCS Core and BMF data sets.

## Installation

You can install the development version of nccsdata from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("UrbanInstitute/nccsdata")
library(nccsdata)
```

## Usage

### Data Pulls

The [`nccsdata`](https://urbaninstitute.github.io/nccsdata/) package can
be used to download legacy core data from 1989 to 2019 for charities,
nonprofits, or private foundations that file their respective required
IRS forms such as Form 990, 990EZs, or both.

This data can be filtered based on
[NTEE](https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md)
codes and geography.

``` r
core_2005_nonprofit_pz <- nccsdata::get_data(dsname = "core",
                                             time = "2005",
                                             scope.orgtype = "NONPROFIT",
                                             scope.formtype = "PZ")
#> Requested files have a total size of 82.6 MB. Proceed
#>                       with download? Enter Y/N (Yes/no/cancel)
#> Warning: The following named parsers don't match the column names: default
#> Warning: One or more parsing issues, call `problems()` on your data frame for details,
#> e.g.:
#>   dat <- vroom(...)
#>   problems(dat)


tibble::as_tibble(core_2005_nonprofit_pz)
#> # A tibble: 157,211 × 150
#>    NTEECC new.code   type.org broad.category major.group univ  hosp  two.digit
#>    <chr>  <chr>      <chr>    <chr>          <chr>       <lgl> <lgl> <chr>    
#>  1 J40    RG-HMS-J40 RG       HMS            J           FALSE FALSE 40       
#>  2 W30    RG-PSB-W30 RG       PSB            W           FALSE FALSE 30       
#>  3 W30    RG-PSB-W30 RG       PSB            W           FALSE FALSE 30       
#>  4 W30    RG-PSB-W30 RG       PSB            W           FALSE FALSE 30       
#>  5 W30    RG-PSB-W30 RG       PSB            W           FALSE FALSE 30       
#>  6 Y42    RG-MMB-Y42 RG       MMB            Y           FALSE FALSE 42       
#>  7 S41    RG-PSB-S41 RG       PSB            S           FALSE FALSE 41       
#>  8 N60    RG-HMS-N60 RG       HMS            N           FALSE FALSE 60       
#>  9 S41    RG-PSB-S41 RG       PSB            S           FALSE FALSE 41       
#> 10 S41    RG-PSB-S41 RG       PSB            S           FALSE FALSE 41       
#> # ℹ 157,201 more rows
#> # ℹ 142 more variables: further.category <int>, division.subdivision <chr>,
#> #   broad.category.description <chr>, major.group.description <chr>,
#> #   code.name <chr>, division.subdivision.description <chr>, keywords <chr>,
#> #   further.category.desciption <chr>, ntee2.code <chr>, EIN <chr>,
#> #   TAXPER <dbl>, STYEAR <dbl>, CONT <dbl>, DUES <dbl>, SECUR <dbl>,
#> #   SALESEXP <dbl>, INVINC <dbl>, SOLICIT <dbl>, GOODS <dbl>, GRPROF <dbl>, …
```

``` r
core_2005_artnonprofits_newyork <- nccsdata::get_data(dsname = "core",
                                                      time = "2016",
                                                      scope.orgtype = "NONPROFIT",
                                                      scope.formtype = "PZ",
                                                      ntee = "ART",
                                                      geo.state = "NY")
#> Requested files have a total size of 113.6 MB. Proceed
#>                       with download? Enter Y/N (Yes/no/cancel)
tibble::as_tibble(core_2005_artnonprofits_newyork)
#> # A tibble: 346 × 168
#>    NTEECC new.code   type.org broad.category major.group univ  hosp  two.digit
#>    <chr>  <chr>      <chr>    <chr>          <chr>       <lgl> <lgl> <chr>    
#>  1 A01    AA-ART-A00 AA       ART            A           FALSE FALSE 1        
#>  2 A01    AA-ART-A00 AA       ART            A           FALSE FALSE 1        
#>  3 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#>  4 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#>  5 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#>  6 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#>  7 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#>  8 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#>  9 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#> 10 A03    PA-ART-A00 PA       ART            A           FALSE FALSE 3        
#> # ℹ 336 more rows
#> # ℹ 160 more variables: further.category <int>, division.subdivision <chr>,
#> #   broad.category.description <chr>, major.group.description <chr>,
#> #   code.name <chr>, division.subdivision.description <chr>, keywords <chr>,
#> #   further.category.desciption <chr>, ntee2.code <chr>, EIN <chr>,
#> #   ACCPER <chr>, ACTIV1 <chr>, ACTIV2 <chr>, ACTIV3 <chr>, ADDRESS <chr>,
#> #   AFCD <dbl>, ASS_BOY <dbl>, ASS_EOY <dbl>, BOND_BOY <dbl>, BOND_EOY <dbl>, …
```

- Full
  [`get_data()`](https://urbaninstitute.github.io/nccsdata/articles/data_pull.html)
  vignette

### Summarising Data

After processing the desired data,
[`nccsdata`](https://urbaninstitute.github.io/nccsdata/) can also be
used to generate summary tables.

``` r
nccsdata::preview_sample(data = core_2005_artnonprofits_newyork,
                         group_by = c("NTEECC", "STATE"),
                         var = c("TOTREV"),
                         stats = c("count", "mean", "max"))
#> # A tibble: 29 × 5
#> # Groups:   NTEECC [29]
#>    NTEECC STATE count     mean     max
#>    <chr>  <chr> <int>    <dbl>   <dbl>
#>  1 A01    NY        2   77734   151889
#>  2 A03    NY       14  924422. 9222403
#>  3 A11    NY        2  762752. 1485739
#>  4 A19    NY        1   50300    50300
#>  5 A20    NY        5  236864.  711793
#>  6 A23    NY      112   64598.  758835
#>  7 A30    NY       26  810943. 4974965
#>  8 A31    NY        3 1389737. 2142396
#>  9 A32    NY        7  759396. 3154923
#> 10 A33    NY       15  329639.  828684
#> # ℹ 19 more rows
```

- Full
  [`preview_sample()`](https://urbaninstitute.github.io/nccsdata/articles/summary_stats.html)
  vignette.

### NTEE Codes

[`nccsdata`](https://urbaninstitute.github.io/nccsdata/) also offers
several supplementary functions for documenting and retrieving
[NTEE](https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md)
codes.

- Full [`ntee_preview()` and
  `parse_ntee()`](https://urbaninstitute.github.io/nccsdata/articles/ntee.html)
  vignette.

## Getting Help

Raise an issue on the
[issues](https://github.com/UrbanInstitute/nccsdata/issues) page or
contact Thiyaghessan at `tpoongundranar@urban.org`.
