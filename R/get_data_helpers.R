#' @export
validate_get_data <- function(dsname = NULL,
                              time = NULL,
                              scope.orgtype = NULL,
                              scope.formtype = NULL,
                              ntee = NULL,
                              geo = NULL){

  stopifnot(
    "Invalid datatype. dsname must be a character scalar." =
      is_scalar_character(dsname) == TRUE,
    "Invalid dataseries. Select either 'core' or 'bmf'." =
      (dsname == "core" | dsname == "bmf"),
    "Invalid datatype. time must be a character vector." =
      is_character(time) == TRUE,
    "Invalid date range specified. Choose years from 1989-2022." =
      all(as.numeric(time) %in% seq(1989, 2022))

    )
}
