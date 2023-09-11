#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param formula PARAM_DESCRIPTION
#' @param data PARAM_DESCRIPTION
#' @param subset PARAM_DESCRIPTION
#' @param weights PARAM_DESCRIPTION
#' @param na.action PARAM_DESCRIPTION
#' @param method PARAM_DESCRIPTION, Default: 'qr'
#' @param model PARAM_DESCRIPTION, Default: TRUE
#' @param x PARAM_DESCRIPTION, Default: FALSE
#' @param y PARAM_DESCRIPTION, Default: FALSE
#' @param qr PARAM_DESCRIPTION, Default: TRUE
#' @param singular.ok PARAM_DESCRIPTION, Default: TRUE
#' @param contrasts PARAM_DESCRIPTION, Default: NULL
#' @param offset PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stats]{model.frame}}
#' @rdname lm
#' @export
#' @importFrom data.table setDT
#' @importFrom stringr str_replace
#' @import dtplyr
#' @import dplyr

get_data <- function(ntee.level1 = "all",
                     ntee.level2 = "all",
                     geo.state = NULL,
                     geo.metro = NULL,
                     geo.level = "tract"){

  # load in datasets as data.table
  tinybmf_dat <- data.table::setDT(tinybmf)
  tract_dat <- data.table::setDT(tract_dat)
  block_dat <- data.table::setDT(block_dat)
  ntee_dat <- data.table::setDT(ntee_df)
  cbsa_dat <- data.table::setDT(cbsa_df)

  # Data wrangling

  # bmf data

  tinybmf_dat <- tinybmf_dat %>%
    dplyr::rename(tract.census.geoid = TRACT.GEOID.10,
                  block.census.geoid = BLOCK.GEOID.10,
                  state.census.abbr = STATE,
                  ntee2.code = NTEE2) %>%
    dplyr::mutate(across(c("tract.census.geoid", "block.census.geoid"),
                          stringr::str_replace,
                          "GEO-",
                          ""))
  # cbsa data

  cbsa_ex_cols <- setdiff(colnames(cbsa_df), colnames(tract_dat))
  cbsa_dat <- cbsa_dat %>%
    dplyr::select(append("metro.census.cbsa.geoid", cbsa_ex_cols)) %>%
    group_by(metro.census.cbsa.geoid)

  # Apply NTEE filters
  if (any(! ntee.level1 == "all" | ! ntee.level2 == "all")){
  ntee2_codes <- parse_ntee(ntee.group = ntee.level1,
                            ntee.code = ntee.level2,
                            ntee.orgtype = "all")
  tinybmf_subset <- tinybmf_dat %>%
    dplyr::filter(ntee2.code %in% ntee2_codes) %>%
    dplyr::left_join(ntee_dat, by = "ntee2.code")
  } else {
    tinybmf_subset <- tinybmf_dat %>%
      dplyr::left_join(ntee_dat, by = "ntee2.code")
  }

  # Apply geographic filters


  # State filter
  if (! is.null(geo.state)) {
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::filter(state.census.abbr %in% geo.state)
  }

  # Census level
  if (geo.level == "tract"){
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(tract_dat, by = "tract.census.geoid")
  } else if (geo.level == "block") {
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(block_dat, by = "block.census.geoid")
  } else if (geo.level == "both") {
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(tract_dat, by = "tract.census.geoid") %>%
      dplyr::left_join(block_dat, by = "block.census.geoid")
  } else {
    stop("Invalid geo.level, select either 'block', 'tract' or 'both'")
  }

  # CBSA filter
  if (! is.null(geo.metro)){
    tinybmf_subset <- tinybmf_subset %>%
      dplyr::left_join(tract_dat, by = "tract.census.geoid") %>%
      dplyr::select(metro.census.cbsa.geoid %in% geo.metro) %>%
      dplyr::group_by("metro.census.cbsa.geoid") %>%
      dplyr::left_join(cbsa_dat, by = "metro.census.cbsa.geoid") %>%
      dplyr::ungroup()
  }


  return(tinybmf_subset)

}
