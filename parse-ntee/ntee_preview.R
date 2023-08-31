source("parse_ntee.R")
library("dplyr")
library("reactable")

#' Function to preview NTEE codes
#' 

ntee_preview <- function(ntee.group = "all",
                         ntee.code = "all",
                         ntee.orgtype = "all",
                         visualize = FALSE){
  
  ntee2_codes <- parse_ntee(ntee.group = ntee.group,
                            ntee.code = ntee.code,
                            ntee.orgtype = ntee.orgtype)
  
  ntee_df <- readRDS("../data-raw/ntee_df.RDS")  
  
  filtered_df <- ntee_df %>% 
    dplyr::filter(ntee2.code %in% ntee2_codes[[1]])
  
  if (visualize == FALSE){
    return(filtered_df)
  } else {
    return(reactable(filtered_df))
  }
  
}
