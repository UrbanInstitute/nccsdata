#' @title Provide documentation for NTEE2 codes
#'
#' @description Take user inputs across levels 1- 5 of the NTEE2 code structure
#' and returns text metadata describing each NTEE Code level
#'
#' @param ntee character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
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
#' @usage ntee_preview(ntee, ntee.group, ntee.code, ntee.orgtype)
#'
#' @returns text strings that describe NTEE2 codes at each level
#'
#' @examples
#' ntee_preview(ntee = c("ART", "A2X"))
#'
#' @export

ntee_preview <- function(ntee = NULL,
                         ntee.group = NULL,
                         ntee.code = NULL,
                         ntee.orgtype = NULL){

  ntee2_codes <- query_ntee(ntee.user = ntee,
                            ntee.group = ntee.group,
                            ntee.code = ntee.code,
                            ntee.orgtype = ntee.orgtype)

  metadata <- ntee_metadata(ntee.user = ntee2_codes)

  full_desc <- ""
  current_group <- ""
  current_org <- ""

  for (ntee_full in metadata$ntee.user){

    group <- ntee_full[1]
    orgtype <- ntee_full[2]
    code <- ntee_full[3]

    if (group != current_group){

      current_group <- group
      current_org <- ""
      group_statement <- sprintf("%s: %s", group, metadata$group[group])
      cat("\n\n", group_statement)

    }

    if  (orgtype != current_org){

      current_org <- orgtype
      org_desc <- strwrap(metadata$org[orgtype])

      org_statement <- sprintf("    %s: %s", orgtype, org_desc[1])
      cat("\n\n", org_statement)

      if (length(org_desc) > 1){

        org_statement <- sprintf("        %s", org_desc[2])
        cat("\n", org_statement)

      }


      }

    code_statement <- sprintf("        %s: \n", code)
    cat("\n\n", code_statement)
    for (str in strwrap(metadata$code[code])){

      code_statement <- sprintf("        %s", str)
      cat("\n", code_statement)

    }

  }

  cat("\n\n")
  return(message("End of preview."))

  }

#' @title Parse NTEE2 Codes
#'
#' @description Take arguments for Industry Group, Industry, Division,
#' Subdivision and Organization Types and filters population of NTEE2 codes to
#' return matching codes
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
