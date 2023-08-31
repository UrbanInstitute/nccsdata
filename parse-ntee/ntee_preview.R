source("parse_ntee.R")
library("dplyr")
library("reactable")

#' Function to interactively explore NTEE codes
#' 
#' @description 
#' 

ntee_preview <- function(ntee.group = "all",
                         ntee.code = "all",
                         ntee.orgtype = "all",
                         cols = "all",
                         visualize = FALSE){
  
  ntee2_codes <- parse_ntee(ntee.group = ntee.group,
                            ntee.code = ntee.code,
                            ntee.orgtype = ntee.orgtype)
  
  ntee_df <- readRDS("../data-raw/ntee_df.RDS")
  
  # Specify columns to select
  
  if (any(cols == "all")){
    col_names = colnames(ntee_df)
  } else {
    col_names = cols
  }
  
  # Filter ntee_df
  
  filtered_df <- ntee_df %>% 
    dplyr::filter(ntee2.code %in% ntee2_codes[[1]]) %>% 
    dplyr::select(dplyr::any_of(col_names))
  
  # Decide on output format
  if (visualize == FALSE){
    return(filtered_df)
  } else {
    return(reactable(filtered_df))
  }
  
}
