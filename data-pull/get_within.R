get_within(within_args){
  
  # save vectors containing elements from subsettable columns
  # construct filters based on these elements
  # apply filters to dataframee
  
}

library("dplyr")
test_vec <- c("1001")

abbr_vec <- c("NY", "CA")

length(intersect(test_vec, abbr_vec))


cbsa_df <- readRDS("data-raw/cbsa_df.RDS")
cbsa_filter <- cbsa_df %>% 
  dplyr::filter(if_all(.cols = dplyr::everything(),
                       .fns = function(x) x %in% c("AL", "Alabama")))
