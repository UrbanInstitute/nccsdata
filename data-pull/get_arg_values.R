# Load packages
library("reactable")

#' This function filters the cbsa dataframe.
#' 
#' @description This function takes in a cbsa dataset and a dynamic list
#' of user selected columns. It returns a filtered dataframes based on user
#' conditions
#' 
#' @param dataset string. Name of dataset to load, "cbsa", "block", "tract".
#' @param visual boolean. Option to return reactable visualization or filtered
#' dataframe
#' @param ... expression. User inputs of selected columns and values to filter
#' by. E.g. (state.census.abbr = c("NY", "AL")). Leaving blank returns all 
#' columns
#' 
#' @usage get_arg_values(dataset, visual)
#' 
#' @returns filtered dataframe or reactable based on  user-inputs
#' 
#' @examples 
#' get_arg_values("cbsa", TRUE, state.census.name = c("Wyoming", "Montana"))
#' get_arg_values("tract", TRUE, 
#'                 metro.census.cbsa.geoid = c("10100", "10200"), 
#'                 state.census.abbr = c("NY", "CA"))
#' @export

get_arg_values <-  function(dataset, 
                            visual = FALSE,
                            within = NULL,
                            ...){
  
  # Read RDS
  RDS_ls <- readRDS("data-raw/data_directory.RDS")
  cbsa_df <- readRDS(paste0("data-raw/",
                            RDS_ls[[dataset]]))
  
  # Variable constructor
  
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