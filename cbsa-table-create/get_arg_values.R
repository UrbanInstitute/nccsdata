#' This function filters the cbsa data and returns a table.
#' 

get_arg_values <-  function(...){
  
  # Read RDS
  cbsa_df <- readRDS("cbsa_df.RDS")
  
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
  
  return(filtered_df)
}
