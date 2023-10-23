setwd( "~/nccs-data-package/nccsdata" )

library( usethis )

## ADD DATA TO PACKAGE

tinybmf <- read.csv( "data-raw/tinybmf.csv" )
use_data( tinybmf )

sinew::makeOxygen( tinybmf )

devtools::document()


## Raw Data


### CBSA and CSA Definitions 

Core based statistical areas (CBSAs), metropolitan divisions, and combined statistical areas (CSAs)

https://www2.census.gov/programs-surveys/metro-micro/geographies/reference-files/2013/delineation-files/list1.xls

named: cbsa_csa_definition_file_feb_2013.xls

After wrangling it is renamed: metrofips.csv

