#' Function to validate user inputs
validate_arg <- function(dat, args = args, ex_args = ex_args, id_col,
                         geo.level){
  if (all(names(args) %in% colnames(dat))){
    parsed_ids <- dat %>% 
      suppressWarnings(filter(!!! ex_args)) %>% 
      select(id_col)
    return(parsed_ids)
  } else {
    absent_colnames <- setdiff(names(args), colnames(dat))
    stop(paste("The following columns are not present in the",
               geo.level,
               "dataset:",
               setdiff(names(args), colnames(dat))))
  }  
}
