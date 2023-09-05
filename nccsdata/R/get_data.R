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
#' @importFrom dtplyr rename
#' @importFrom dtplyr mutate
#' @importFrom dtplyr filter
#' @importFrom stringr str_replace

get_data <- function(state){

  # load in tinybmf data as data.table
  load("data/tinybmf.rda")
  tinybmf_dat <- data.table::setDT(tinybmf)

  # load in tract data as data.table
  load("data/tract_dat.rda")
  tract_dat <- data.table::setDT(tract_dat)

  # load in block data as data.table
  load("data/block_dat.rda")
  block_dat <- data.table::setDT(block_dat)

  # load in ntee data as data.table
  load("data/ntee_df.rda")
  ntee_dat <- data.table::setDT(ntee_dat)

  # rename columns, wrangle data and filter
  tinybmf_dat <- tinybmf_dat %>%
    dtplyr::rename(tract.census.geoid = TRACT.GEOID.10,
                   state.census.abbr = STATE) %>%
    dtplyr::mutate(across("tract.census.geoid",
                          stringr::str_replace,
                          "GEO-",
                          "")) %>%
    dtplyr::filter(state.census.abbr %in% state)

  tract_dat <- tract_dat %>%
    dtplyr::mutate(tract.census.geoid = as.character(
                                         as.numeric(tract.census.geoid)
                                         ))


}
