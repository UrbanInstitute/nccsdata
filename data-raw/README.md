setwd( "~/nccs-data-package/nccsdata" )

library( usethis )

## ADD DATA TO PACKAGE

tinybmf <- read.csv( "data-raw/tinybmf.csv" )
use_data( tinybmf )

sinew::makeOxygen( tinybmf )

devtools::document()
