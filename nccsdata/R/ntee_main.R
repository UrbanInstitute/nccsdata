#' Script with front end functions for users to access NTEE2 codes and
#' filter NTEE dataset with codes
#'

#' Function to interactively explore NTEE codes
#'
#' @description This function takes user inputs across levels 1- 5 of the
#' NTEE2 code structure and returns selected columns from the NTEE database.
#' The use can choose to return a dataframe or reactable object.
#'
#' @param ntee.group character vector. Vector of desired Industry Group codes
#'  to filter. Use "all" to include all possible codes.
#'
#' @param ntee.code character vector. Sequence of desired Industry, Division and
#' Subdivision codes (old code structure) to use in filtering. Use "all" to
#' include all possible codes. Can also provide only partial codes. For example
#' "A" or "Axx" will query NTEE2 codes based on Industry group "A" and all
#' division and subdivisions.
#'
#' @param ntee.orgtype character vector. Vector of Organization Types.
#' Use "all" to include all possible codes.
#'
#' @param cols character vector. A vector containing list of columns to select.
#' Default value is "all" for all columns.
#'
#' @param visualize boolean. A boolean value indicating whether to return a
#' reactable object or data frame.
#'
#' @usage ntee_preview(ntee.group, ntee.code, ntee.orgtype, cols, visualize)
#'
#' @returns a dataframe or reactable object with rows that match user arguments.
#'
#' @examples
#' ntee_preview("all", "A1x", "all", "all",TRUE)
#' ntee_preview("ART", "Axx", "all", "all",TRUE)
#' ntee_preview("all", "Axx", "all", c("type.org", "univ"), FALSE)
#'
#' @import dplyr
#' @import reactable
#' @importFrom dplyr %>%
#'
#' @export

ntee_preview <- function(ntee.group = "all",
                         ntee.code = "all",
                         ntee.orgtype = "all",
                         cols = "all",
                         visualize = FALSE){

  ntee2_codes <- parse_ntee(ntee.group = ntee.group,
                            ntee.code = ntee.code,
                            ntee.orgtype = ntee.orgtype)

  # Specify columns to select

  if (any(cols == "all")){
    col_names = colnames(ntee_df)
  } else {
    col_names = cols
  }

  # Filter ntee_df

  filtered_df <- ntee_df %>%
    dplyr::filter(ntee2.code %in% ntee2_codes) %>%
    dplyr::select(dplyr::any_of(col_names))

  # Decide on output format
  if (visualize == FALSE){
    return(filtered_df)
  } else {
    return(reactable(filtered_df))
  }

}


#' A complete function takes any user input values from the arguments:

#'    ntee.group
#'    ntee.code
#'    ntee.orgtype

#' And returns the correct list of NTEE codes that match the filter
#' requirements.

#' It will also raise an error if the user tries argument values that are
#' undefined and print an informative message.
#'
#' Function that generates population of NTEE2 codes and filters codes that
#' match user inputs
#'
#' @description This function takes user defined Industry Group, Industry,
#' Division, Subdivision and Organization Types and filters population of NTEE2
#' codes to find codes that match user inputs.
#'
#' @param ntee.group character vector. Vector of desired Industry Group codes
#'  to filter. Use "all" to include all possible codes.
#' @param ntee.code character vector. Sequence of desired Industry, Division and
#' Subdivision codes (old code structure) to use in filtering. Use "all" to
#' include all possible codes. Can also provide only partial codes. For example
#' "A" or "Axx" will query NTEE2 codes based on Industry group "A" and all
#' division and subdivisions.
#' @param ntee.orgtype character vector. Vector of Organization Types.
#' Use "all" to include all possible codes.
#'
#' @usage parse_ntee(ntee.group, ntee.code, ntee.orgtype)
#'
#' @returns list of matched NTEE2 codes
#'
#' @export

parse_ntee <- function(ntee.group, ntee.code, ntee.orgtype){

  # Validate user inputs
  validate_inp(
    ntee.group = ntee.group,
    ntee.code = ntee.code,
    ntee.orgtype = ntee.orgtype,
    ind_group_codes = ntee_df$broad.category,
    level_2_4_codes = ntee_df$old.code,
    org_type_codes = ntee_df$type.org
  )

  # Generate regex queries if inputs are valid
  regex_queries <- generate_ntee_regex(
    ntee.group = ntee.group,
    ntee.code = ntee.code,
    ntee.orgtype = ntee.orgtype
  )

  # Execute regex queries
  ntee2_codes <- parse_ntee_regex(
    regexp_vec = regex_queries,
    ntee_codes = ntee_df$ntee2.code
  )

  # Return NTEE2 Codes
  return(ntee2_codes)

}
