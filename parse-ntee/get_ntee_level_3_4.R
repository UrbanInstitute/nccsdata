#' Function to generate level 3 and 4 of NTEE2 code
#' 
#' @description This function takes in digits23 and digits45 from the old
#' NTEE codes to create levels 3 and 4 of NTEE2 code
#' 
#' @param digits23 character. Digits in the 2nd and 3rd place of old code
#' @param digits45 character. Digits in the 4th and 5th place of old code
#' 
#' @usage get_ntee_level_3_4(digits23, digits45)
#' 
#' @return The level 3 and 4 codes concatenated together in a string
#' 
#' @examples 
#' get_ntee_level_3_4("23", "45")
#' get_ntee_level_3_4("03", "22")
#' 
#' @note 
#' See https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md
#' for more details

get_ntee_level_3_4 <- function(digits23, digits45){
  
  ntee2_level_3_4 <- ifelse(
    as.numeric(digits23) > 19,
    substring(digits23, 1, 2),
    substring(digits45, 1, 2)
  )
  
  ntee2_level_3_4 <- ifelse(
    is.na(ntee2_level_3_4),
    substring(digits23, 1, 2),
    ntee2_level_3_4
  )
  
  return(ntee2_level_3_4) 
}
