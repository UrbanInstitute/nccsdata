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
                     geo.region = NULL){

  # load in datasets as data.table
  tinybmf_dat <- data.table::setDT(tinybmf)
  tract_dat <- data.table::setDT(tract_dat)
  block_dat <- data.table::setDT(block_dat)
  load("data/ntee_df.rda")
  ntee_dat <- data.table::setDT(ntee_disagg_df)
  cbsa_dat <- data.table::setDT(cbsa_df)

  # rename columns, and wrangle data
  tinybmf_dat <- tinybmf_dat %>%
    dplyr::rename(tract.census.geoid = TRACT.GEOID.10,
                   state.census.abbr = STATE,
                   ntee2.code = NTEE2) %>%
    dplyr::mutate(across("tract.census.geoid",
                          stringr::str_replace,
                          "GEO-",
                          ""))
  # Apply NTEE filters
  if (! ntee.level1 == "all" | ! ntee.level2 == "all"){
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
      dplyr::filter(state.census.abbr %in% geo.state) %>%
      dplyr::left_join(tract_dat, by = "tract.census.geoid")
  }

  return(tinybmf_subset)

}
