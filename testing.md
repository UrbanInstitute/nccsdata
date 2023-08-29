## Test data frame

set.seed( 1234 )

get_test_core <- function()
{
  df <- load(CORE-2010)
  return(df)
}

testcore <- dplyr::sample_n( get_test_df(), 25000 )
# save as object
#  save to "data" folder: "../data/testcore.rda"


get_test_bmf <- function()
{
  df <- load(BMF)
  return(df)
}

testbmf <- dplyr::sample( get_test_bmf(), 25000 )




## Unit Tests

* ntee arguments?
* geo arguments?
* predictable user errors
* edge cases?
  - non-nested geographies, null set
