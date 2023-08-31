#' Check if ALL objects named one vector exist in memory
#' 
#' @description This function is used to check if both block and tract data
#' tables exist in memory
#' 
#' @param ... vector of objects.
#' 
#' @usage objs_exist(c("block_dt", "tract_dt"))
#' 
#' @return A single boolean value indicating whether all objects are present 
#' in memory

objs_exist <- function(...) {
  
  ls <- list(...)
  all(sapply(ls, exists))
  
}