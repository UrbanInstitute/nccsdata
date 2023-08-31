# Load packages
library("reactable")

#' This function filters the cbsa dataframe.
#' 
#' @description This function takes in a cbsa dataset and a dynamic list
#' of user selected columns. It returns a filtered dataframes based on user
#' conditions
#' 
#' 

get_arg_values <-  function(dataset, visual = TRUE, ...){
  
  # Read RDS
  RDS_ls <- readRDS("data-raw/data_directory.RDS")
  cbsa_df <- readRDS(RDS_ls[[dataset]])
  
  # Create filter expressions
  filter_conditions <- enquos(...)
  filter_conditions_exp <- unname(purrr::imap(
    filter_conditions,
    function(expr, name) quo( !! sym(name) == !! expr)
  )
  )
  
  # Create filtered DF
  filtered_df <- suppressWarnings(dplyr::filter(cbsa_df,
                                                !!! filter_conditions_exp))  
  if (visual == TRUE){
    return(reactable(filtered_df))
  } else {
    return(filtered_df) 
  }
}
