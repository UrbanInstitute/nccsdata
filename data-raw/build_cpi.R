## Build the bundled `cpi_u` annual CPI-U series.
## Run from the package root:
##   Rscript data-raw/build_cpi.R
##
## Source: FRED series CPIAUCNS (BLS CPI-U, U.S. city average, all items,
## NSA), aggregated to annual averages by FRED. Public domain BLS data.

fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCNS&fq=Annual"
tmp <- tempfile(fileext = ".csv")
utils::download.file(fred_url, tmp, mode = "wb", quiet = TRUE)

raw <- utils::read.csv(tmp, stringsAsFactors = FALSE)
stopifnot(identical(names(raw), c("observation_date", "CPIAUCNS")))

cpi_u <- dplyr::tibble(
  year = as.integer(format(as.Date(raw$observation_date), "%Y")),
  cpi  = as.numeric(raw$CPIAUCNS)
)

# Drop the trailing partial / forthcoming year (no value yet) and any
# rows where FRED hasn't filled in the annual average.
cpi_u <- cpi_u[!is.na(cpi_u$cpi), , drop = FALSE]

# Stamp the snapshot date so users can tell how fresh their copy is.
attr(cpi_u, "fred_series")  <- "CPIAUCNS"
attr(cpi_u, "fetched_on")   <- as.character(Sys.Date())

usethis::use_data(cpi_u, overwrite = TRUE)
