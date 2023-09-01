#' Script with utility functions

#' Function that prettifies json
#' 
#' @import jsonlite

jsonify_f <- function(f)
{
  f <- as.factor(f)
  f_level <- levels(f)
  f_level <- gsub( "'", "", f_level ) # remove quotes
  f_level <- gsub( '"', '', f_level ) # remove apostrophes
  label <- f_level
  d <- data.frame(f_level,label)
  jd <- jsonlite::toJSON( d )
  jd <- gsub( "\\{", "  \\{  ", jd )
  jd <- gsub( ":", " :  ", jd )
  jd <- gsub( '",', '"  ,  ', jd )
  jd <- gsub( '","', '",  "', jd )
  jd <- gsub( "\\},", "  \\}, \n", jd )
  jd <- gsub( "\\[", "\\[ \n", jd )
  jd <- gsub( "\\]", "\n\\]", jd )
  jd <- gsub( "\\}\n", "  \\}\n", jd )
  return(jd)
}

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