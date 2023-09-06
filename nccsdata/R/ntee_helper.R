#' Script containing helper functions for ntee_main.R
#'
#'
#' This function preprocesses the disaggregated NTEE dataset and returns
#' a vector of NTEE codes formatted according to NTEE2 standards.
#'
#' @description This function reads in a csv containing disaggregated NTEE
#' codes built from the old formatting. It uses the rules specified in
#' https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md
#' to derive the codes from level 1-5 used in NTEE2 from old NTEE codes
#'
#' @param path_to_csv character. Path to the .csv file with disaggregated NTEE
#' data
#'
#' @usage ntee_preproc(path_to_csv)
#'
#' @return A list containing the population of valid NTEE2 codes
#'
#' @note The disaggregated csv file can be edited and the function rerun
#' to generate a new list of valid NTEE2 codes if needed.
#'
#' @import dplyr
#' @import data.table

ntee_preproc <- function(path_to_rda = "data/ntee_disagg_df.rda"){

  # Read csv file to extract different versions of NTEE Codes
  load(path_to_rda)

  # Extract level 1 and level 2 parts of NTEE2 Code
  ntee2_level1 <- ntee_disagg_df$broad.category
  ntee2_level2 <- ntee_disagg_df$major.group

  # Extract digits23 and digits 45
  digits23 <- substring(ntee_disagg_df$old.code, 2, 3)
  digits45 <- substring(ntee_disagg_df$old.code, 4, 5)
  digits45 <- replace(digits45, digits45 == "", "00")

  # Use digits23 and digits45 to get level 3 and 4 of new NTEE code
  ntee2_level_3_4 <- mapply(get_ntee_level_3_4, digits23, digits45)

  # Combine levels 2-4
  ntee2_level_2_4 <- paste(ntee2_level2, ntee2_level_3_4, sep = "")

  # Extract level 5 code from disaggregated csv
  ntee2_level5 <- ntee_disagg_df$type.org

  # Create population of NTEE2 codes
  ntee_new_codes <- paste(
    ntee2_level1,
    "-",
    ntee2_level2,
    ntee2_level_3_4,
    "-",
    ntee2_level5,
    sep = ""
  )

  # Append NTEE2 codes to disaggregated csv and save
  ntee_disagg_df$ntee2.code <- ntee_new_codes
  save(ntee_disagg_df, file = "data/ntee_df.rda")

  return(list(ntee_new_codes,
              ntee2_level1,
              ntee2_level_2_4,
              ntee2_level5))
}

#' This function returns a regex query to parse NTEE2 Codes
#'
#' @description This function creates the regex query that will be used
#' to filter NTEE2 codes based on user inputs.
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
#' @usage generate_ntee_regex(ntee.group, ntee.code, ntee.orgtype)
#'
#' @return regex query that can be used in parse_ntee() function to filter
#' population of NTEE2 codes.
#'
#' @examples
#' generate_ntee_regex("ART", "A23", "RG")
#' generate_ntee_regex("all", "Axx", "RG")
#' generate_ntee_regex("EDU", "B", "all")
#'
#' @export

generate_ntee_regex <- function(ntee.group, ntee.code, ntee.orgtype){
  # Formulate regex query based on user input

  level1_query <- ifelse(
    ntee.group == "all",
    "[A-Z][A-Z][A-Z]",
    ntee.group
  )

  level_2to4_query <- ifelse(
    ntee.code == "all",
    "[A-Z][0-9][A-Z0-9]",

    ifelse(
      grepl("^[A-Z]$", ntee.code) | grepl("^[A-Z][xX]*[xX]$", ntee.code),
      paste(substring(ntee.code, 1, 1), "[0-9][A-Z0-9]", sep = ""),

      ifelse(
        grepl("^[A-Z][0-9]$", ntee.code) | grepl("^[A-Z][0-9][xX]$", ntee.code),
        paste(substring(ntee.code, 1, 2), "[A-Z0-9]", sep = ""),
        ntee.code
      )
    )
  )

  level_5_query <- ifelse(
    ntee.orgtype == "all",
    "[A-Z][A-Z]",
    ntee.orgtype
  )

  full_query <- data.table::CJ(
    level1_query,
    level_2to4_query,
    level_5_query
  )[, paste(level1_query,
            level_2to4_query,
            level_5_query,
            sep = "-")]

  return(full_query)
}

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
#'
#' @export

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

#' Function to loop through and execute a vector of regex queries
#' on a vector of ntee codes
#'
#' @description This function executes a vector of regex queries on
#' a vector containing the population of NTEE2 codes
#'
#' @param regexp_vec character vector. Vector containing all regex queries
#' @param ntee_codes character vector. Vector containing population of NTEE2
#' codes
#'
#' @usage parse_ntee_regex(regexp_vec, ntee_codes)
#'
#' @return vector of matched NTEE2 codes

parse_ntee_regex <- function(regexp_vec, ntee_codes){

  matched_codes <- c()

  for (regexp in regexp_vec){
    results <- ntee_codes[grep(regexp, ntee_codes)]
    matched_codes <- c(results, matched_codes)
  }
  return(unique(matched_codes))

}

#' Function to inspect user inputs and flag errors
#'
#' @description This function validates user inputs for Industry Group,
#' Industry, Division, Subdivision and Organization Type, comparing them
#' to the set of codes in the population. It throws informative error
#' messages.
#'
#' @param ntee.group character. 3-character alphabetical Industry Group code.
#' @param ntee.code character. 3-character alphanumeric containing
#' Industry, Division and Subdivision
#' @param ntee.orgtype character. 2-character alphabetical Organization Type
#' code.
#' @param ind_group_codes. character vector of all acceptable Industry Group
#' codes.
#' @param level_2_4_codes. character vector of all acceptable Industry,
#' Division and Subdivision codes.
#' @param org_type_codes. character vector of all acceptable Organization
#' Type codes.
#'
#' @usage validate_inp(ntee.group, ntee.code, ntee.orgtype, ind_group_codes,
#'                     level_2_4_codes, org_type_codes)
#' @return String indicating whether input is valid or invalid. If invalid,
#' points user to a list of acceptable codes.

validate_inp <- function(ntee.group,
                         ntee.code,
                         ntee.orgtype,
                         ind_group_codes,
                         level_2_4_codes,
                         org_type_codes){

  if (any(ntee.group %in% c(ind_group_codes, "all"))){
    message("Collecting Matching Industry Groups")
  } else {
    stop("Invalid Industry Group \n
          List of available groups can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md")
  }

  if (any(ntee.code %in% c(level_2_4_codes, "all")) |
      any(grepl("[A-Z][0-9xX]*[A-Z0-9xX]*", ntee.code))){
    message("Collecting Matching Industry Division and Subdivisions")
  } else {
    stop("Invalid Industry Division Subdivision Combination \n
          List of available Combinations can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md")
  }

  if (any(ntee.orgtype %in% c(org_type_codes, "all"))) {
    message("Collecting Matching Organization Types")
  } else {
    stop("Invalid Organization Type \n
          List of available Organization Types can be found at: \n
          https://github.com/Nonprofit-Open-Data-Collective/mission-taxonomies/blob/main/NTEE-disaggregated/README.md")

  }

}
