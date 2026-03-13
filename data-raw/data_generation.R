## Script to generate bmf_dictionary dataset from CSV
## Run this script from the package root directory:
##   source("data-raw/data_generation.R")

bmf_dictionary <- utils::read.csv(
  "data-raw/bmf_data_dictionary.csv",
  stringsAsFactors = FALSE
)

# Keep only stable metadata columns (drop snapshot stats)
bmf_dictionary <- bmf_dictionary[, c("column_name", "description", "type")]

# Convert to tibble for consistency
bmf_dictionary <- dplyr::as_tibble(bmf_dictionary)

usethis::use_data(bmf_dictionary, overwrite = TRUE)
