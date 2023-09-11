#' This function filters the cbsa dataframe.
#'
#' @description This function takes in a cbsa dataset and a dynamic list
#' of user selected columns. It returns a filtered dataframes based on user
#' conditions
#'
#' @param dataset string. Name of dataset to load, "cbsa", "block", "tract".
#' @param visual boolean. Option to return reactable visualization or filtered
#' dataframe
#' @param within character vector. Vector of column variables to filter by
#' without explicit argument definition, filters all rows with columns
#' containing any of the within arguments. For example within = c("NY",
#' "Alabama") will return all rows containing either NY or Alabama
#' @param ... expression. User inputs of selected columns and values to filter
#' by. E.g. (state.census.abbr = c("NY", "AL")). Leaving blank returns all
#' columns
#'
#' @usage preview_meta(dataset, visual, within)
#'
#' @returns filtered dataframe or list with filtered dataframe and first 20
#' rows of table visualized with reactable
#'
#' @examples
#' preview_meta("cbsa", TRUE, state.census.name = c("Wyoming", "Montana"))
#' preview_meta("tract", TRUE,
#'                 metro.census.cbsa.geoid = c("10100", "10200"),
#'                 state.census.abbr = c("NY", "CA"))
#' preview_meta("tract", TRUE, within = c("NY", "Alabama"))
#'
#' @import purrr
#' @import dplyr
#' @import reactable
#'
#'
#' @export

preview_meta <-  function(dataset,
                            visual = FALSE,
                            within = NULL,
                            ...){

  # Read in data
  if (dataset == "cbsa"){
    data <- cbsa_df
  } else if (dataset == "tract"){
    data <- tract_dat
  } else if (dataset == "block"){
    data <- block_dat
  } else {
    stop("Invalid dataset. Valid inputs include: 'cbsa', 'tract', 'block'")
  }

  # Create filter conditions

  filter_conditions <- enquos(...)
  filter_conditions_exp <- unname(purrr::imap(
    filter_conditions,
    function(expr, name) quo( !! sym(name) == !! expr)
  )
  )

  # Filter DF
  if (is.null(within)){
    filtered_df <- suppressWarnings(dplyr::filter(data,
                                                  !!! filter_conditions_exp))
  } else {
    filtered_df <- cbsa_df %>%
      dplyr::filter(if_any(.cols = dplyr::everything(),
                           .fns = function(x) x %in% within))
  }

  if (visual == TRUE){
    return(list(reactable(head(filtered_df, 20)),
                invisible(filtered_df)))
  } else {
    invisible(filtered_df)
  }
}
