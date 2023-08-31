#' Function to filter data table
#' 
#' @description dat_filter() takes in arguments specified by the user (name of 
#' dataset, column names and subsets), checks if those filters apply to the
#' specified dataset and returns FIPs codes that meet the subsetting conditions.
#' 
#' A user specifies whether they are pulling data from the Block or Tract 
#' datasets and specifies the subset of the data they want e.g. (geo.state.abbr
#' = c("NY")). The function returns either the Tract or Block IDs that fit
#' the subset specified
#' 
#' @param dat data table. Data table of either census or block data.
#' @param args string expression. User arguments specified in parse_geo()
#' @param exp_args string expression. Processed user arguments specified in
#' parse_geo()
#' @param id_col string. Name of column with Tract or Block IDs
#' @geo.level name of data.table (Block/Census) for error messages.
#' 
#' @returns A list of fips codes based on filter criteria.

dat_filter <- function(dat, 
                       args = args, 
                       ex_args = ex_args, 
                       id_col,
                       census.level){
  
  if (all(names(args) %in% colnames(dat))){
    
    parsed_ids <- dat %>% 
      suppressWarnings(filter(!!! ex_args)) %>% 
      select(id_col)
    
    return(parsed_ids)
    
  } else {
    
    absent_colnames <- setdiff(names(args), colnames(dat))
    
    stop(paste("The following columns are not present in the",
               census.level,
               "dataset:",
               setdiff(names(args), colnames(dat))))
  }  
}
