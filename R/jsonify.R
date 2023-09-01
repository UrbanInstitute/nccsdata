#' Function that prettifies json
#' 
#' @export

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