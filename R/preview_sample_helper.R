#' @title Function to validate inputs to preview_sample()
#'
#' @description This function stops execution of preview_sample() if invalid
#' query columns are entered
#'
#' @param df_cols character vector. Vector of columns in 'data' arg in
#' preview_sample(). Used to ensure that query columns are present in data
#' @param group_by character vector. Vector of columns for dplyr::group_by()
#' @param var character scalar. Column to calculate summary statistics with
#' @param stats. character vector. Vector of summary statistics to compute with
#' dplyr::summarise(). Available options are count, min, max, median and mean
#'
#' @returns Informative error messages for the user to edit function arguments
#'
#' @importFrom rlang is_scalar_character

validate_preview <- function(df_cols,
                             group_by,
                             var,
                             stats){

  summary_cols <- c(group_by, var)
  diff_statement <- sprintf("Invalid column names.
                            The following columns are not in %s: %s",
                            deparse(substitute(data)),
                            setdiff(summary_cols, df_cols))

  stopifnot("Can only summarise information for a single variable.
            'Var' must include only one column." = rlang::is_scalar_character(var),

            "Invalid summary statistics.
            Valid options are 'count', 'min', 'median', 'mean' and 'max'" =
              all(stats %in% c('count', 'min', 'median', 'mean', 'max')))

  ifelse(all(summary_cols %in% df_cols),
         return(message("Valid summary fields entered.")),
         stop(diff_statement))

}
