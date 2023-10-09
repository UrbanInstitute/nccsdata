
# nccsdata

<!-- badges: start -->
<!-- badges: end -->

nccsdata provides tools to read, filter and append metadata to publicly available NCCS Core and BMF data sets.

## Installation

You can install the development version of nccsdata from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UrbanInstitute/nccsdata")
```

## Example

This example demonstrates the basic workflow for downloading and filtering 
Nonprofit core data and appending BMF and NTEE data to it:

```{r example, echo=FALSE, message=FALSE, results='hide'}
library(nccsdata)

core_2005 <- nccsdata::get_data(dsname = "core",
                                time = "2005")

core_summary <- preview_sample(data = core_2005,
                               group_by = c("NTEECC", "STATE"),
                               var = "TOTREV",
                               stats = c("count", "mean"))

core_summary

```



