#' Script containing helper functions for ntee_main.R
#'
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
#' \dontrun{
#' generate_ntee_regex("ART", "A23", "RG")
#' generate_ntee_regex("all", "Axx", "RG")
#' generate_ntee_regex("EDU", "B", "all")
#' }

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
#' \dontrun{
#' get_ntee_level_3_4("23", "45")
#' get_ntee_level_3_4("03", "22")
#' }
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

#' @title Function to inspect user inputs and flag errors
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
#' @param ind_group_codes character vector of all acceptable Industry Group
#' codes.
#' @param level_2_4_codes character vector of all acceptable Industry,
#' Division and Subdivision codes.
#' @param org_type_codes character vector of all acceptable Organization
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


#' @title Function to sort user ntee inputs into group, code and organization
#' tyoe
#'
#' @description This function takes as input the ntee= argument from get_data()
#' and sorts user inputs into group, code and organization type. It then
#' combines these user inputs with any pre-existing group, code and organization
#' types.
#'
#' @param ntee.user character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
#' @param ntee.group character vector. Specific Industry Group codes submitted
#' by user
#' @param ntee.code character vector. Specific level 2-4 codes (Industry,
#' Division, Subdivision) submitted by user.
#' @param ntee.orgtype character vector. Specific level 5 codes (Organization
#' Type) submittted by user.
#'
#' @usage sort_ntee(ntee.user, ntee.group, ntee.code, ntee.orgtype)
#'
#' @return a list with all ntee groups, codes and organization types being
#' queried by the user
#'
#' @examples
#' \dontrun{
#'     sort_ntee(ntee.user = c("HEA", "B", "A2x", "C45", "RG", "AA"),
#'               ntee.group = c("ART", "EDU"))
#'     sort_ntee(ntee.user = c("HEA", "B", "A2x", "C45", "RG"),
#'               ntee.code = c("A2x", "Bxx"))
#'     sort_ntee(ntee.user = c("HEA", "B", "A2x", "C45"),
#'               ntee.orgtype = c("AA", "RG"))
#' }

sort_ntee <- function(ntee.user, ntee.group, ntee.code, ntee.orgtype){


  # Case convert
  ntee2 <- ntee.user[ntee.user %in% ntee_df$ntee2.code]

  group <- ntee.user[ntee.user %in% ntee_df$broad.category]
  group <- unique(c(group, ntee.group))
  ntee.user <- setdiff(ntee.user, group)

  orgtype <- ntee.user[ntee.user %in% ntee_df$type.org]
  orgtype <- unique(c(orgtype, ntee.orgtype))
  ntee.user <- setdiff(ntee.user, orgtype)

  code <- ntee.user[grepl("^[A-Z][0-9X]*[A-Z0-9X]*", ntee.user)]
  code <- unique(c(code, ntee.code))

  ntee_sort_ls <- list(group  = group,
                       code = code,
                       orgtype = orgtype,
                       ntee2 = ntee2)

  return(ntee_sort_ls)
}


#' @title function to match user queries to ntee2 codes
#'
#' @description This function runs seperate ntee queries on user inputs and
#' combines the results
#'
#' @param ntee.user character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
#' @param ntee.group character vector. Specific Industry Group codes submitted
#' by user
#' @param ntee.code character vector. Specific level 2-4 codes (Industry,
#' Division, Subdivision) submitted by user.
#' @param ntee.orgtype character vector. Specific level 5 codes (Organization
#' Type) submittted by user.
#'
#' @usage query_ntee(ntee.user, ntee.group, ntee.code, ntee.orgtype)
#'
#' @returns a vector of matched unique ntee2 codes

query_ntee <- function(ntee.user,
                       ntee.group,
                       ntee.code,
                       ntee.orgtype){

  ntee2_matches <- c()

  ntee2_query_ls <- sort_ntee(ntee.user = toupper(ntee.user),
                              ntee.group = toupper(ntee.group),
                              ntee.code = toupper(ntee.code),
                              ntee.orgtype = toupper(ntee.orgtype))

  if (all(lengths(ntee2_query_ls) == 0)){

    ntee2_matches <- NULL

  } else {

    if (length(ntee2_query_ls$group) != 0){

      group_matches <- parse_ntee(ntee.group = ntee2_query_ls$group,
                                  ntee.code = "all",
                                  ntee.orgtype = "all")
      ntee2_matches <- c(ntee2_matches, group_matches)

    }

    if (length(ntee2_query_ls$code) != 0){

      code_matches <- parse_ntee(ntee.group = "all",
                                 ntee.code = ntee2_query_ls$code,
                                 ntee.orgtype = "all")
      ntee2_matches <- c(ntee2_matches, code_matches)

    }

    if (length(ntee2_query_ls$orgtype) != 0){

      orgtype_matches <- parse_ntee(ntee.group = "all",
                                    ntee.code = "all",
                                    ntee.orgtype = ntee2_query_ls$orgtype)
      ntee2_matches <- c(ntee2_matches, orgtype_matches)
    }

    if (length(ntee2_query_ls$ntee2) != 0){

      ntee2_matches <- c(ntee2_matches, ntee2_query_ls$ntee2)

    }

  }

  return(unique(ntee2_matches))

}


#' @title This function maps ntee2 codes to ntee.cc codes
#'
#' @description This function takes as an input ntee2 codes inferred from
#' user inputs in get_data() and maps them to ntee.cc codes for merging
#' found in ntee_df
#'
#' @param ntee.user character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
#' @param ntee.group character vector. Specific Industry Group codes submitted
#' by user
#' @param ntee.code character vector. Specific level 2-4 codes (Industry,
#' Division, Subdivision) submitted by user.
#' @param ntee.orgtype character vector. Specific level 5 codes (Organization
#' Type) submittted by user.
#'
#' @return character vector. ntee.cc codes for merging in the core dataset.
#'
#' @usage nteecc_map(ntee.user, ntee.group, ntee.code, ntee.orgtype)

nteecc_map <- function(ntee.user,
                       ntee.group,
                       ntee.code,
                       ntee.orgtype){

  # NTEE parsing
  ntee2_matches <- query_ntee(ntee.user = ntee.user,
                              ntee.group = ntee.group,
                              ntee.code = ntee.code,
                              ntee.orgtype = ntee.orgtype)

  nteecc_matches <- ntee_df %>%
    dplyr::filter(.data$ntee2.code %in% ntee2_matches) %>%
    dplyr::pull(.data$old.code)

  return(nteecc_matches)
}


#' @title Create metadata from NTEE data set for ntee_preview()
#'
#' @description Metadata generating function for ntee_preview(). Creates
#' named lists for each ntee code level. Processes user inputs to ntee_preview()
#' for easy look up using named lists
#'
#' @param ntee.user character vector. Vector of unique NTEE2 codes.
#'
#' @return named list. Named list contains Level 1, Level 2-4, Level 5 and
#' processed user inputs

ntee_metadata <- function(ntee.user){

  # Level 1
  group_dic <- dic_from_df(df = ntee_df,
                           keycol = "broad.category",
                           valcol = "broad.category.description")
  # Level 5
  org_dic <- dic_from_df(df = ntee_df,
                         keycol = "type.org",
                         valcol = "division.subdivision.description")
  # Level 2-4
  code_dic <- dic_from_df(df = ntee_df,
                          keycol = "ntee2.code",
                          valcol = "further.category.desciption")
  # Seconday descriptions if no description present in further category column
  code_dic_2 <- dic_from_df(df = ntee_df,
                            keycol = "ntee2.code",
                            valcol = "division.subdivision.description")

  for (name in names(code_dic)){

    if (code_dic[[name]] == " "){

      code_dic[[name]] = code_dic_2[[name]]

    }

  }

  names(code_dic) <- sapply(strsplit(names(code_dic), "-"), "[", 2)

  # User Inputs
  ntee2_ls <- strsplit(ntee.user, "-")
  ntee2_ls <- lapply(ntee2_ls,
                     FUN = function(vec) vec <- vec[order(c(1, 3, 2))])
  ntee2_ls <- sort(unlist(lapply(ntee2_ls,
                                 paste,
                                 collapse = "-")))
  ntee2_ls <- strsplit(ntee2_ls, "-")

  return(list(group = group_dic,
              code = code_dic,
              org = org_dic,
              ntee.user = ntee2_ls))

}
