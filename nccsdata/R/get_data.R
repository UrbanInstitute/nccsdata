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

get_data <- function(ntee.level1 = NULL,
                     ntee.level2 = NULL,
                     geo.state = NULL,
                     geo.metro = NULL,
                     geo.region = NULL){

  # load in datasets as data.table
  tinybmf_dat <- data.table::setDT(tinybmf)
  tract_dat <- data.table::setDT(tract_dat)
  block_dat <- data.table::setDT(block_dat)
  ntee_dat <- data.table::setDT(ntee_disagg_df)
  cbsa_dat <- data.table::setDT(cbsa_df)

  # rename columns, wrangle data and filter
  tinybmf_dat <- tinybmf_dat %>%
    dplyr::rename(tract.census.geoid = TRACT.GEOID.10,
                   state.census.abbr = STATE,
                   ntee2.code = NTEE2) %>%
    dplyr::mutate(across("tract.census.geoid",
                          stringr::str_replace,
                          "GEO-",
                          "")) %>%
    dplyr::filter(state.census.abbr %in% state)

  tract_dat <- tract_dat %>%
    dplyr::filter(state.census.abbr %in% state)

  block_dat <- block_dat %>%
    dplyr::mutate(block.census.geoid = as.character(
                                        as.numeric(block.census.geoid)
                                        ))

  # set primary key

  # execute merge

  subset_dat <- tract_dat[tinybmf_dat, on = "tract.census.geoid"]
  subset_dat <- ntee_dat[subset_dat, on = "ntee2.code"]

  return(subset_dat)

}
