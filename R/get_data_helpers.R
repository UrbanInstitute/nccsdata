#' @export
validate_get_data <- function(dsname = NULL,
                              time = NULL,
                              scope.orgtype = NULL,
                              scope.formtype = NULL,
                              ntee = NULL,
                              geo = NULL){

  stopifnot(
    "Invalid datatype. dsname must be a string" =
      is_scalar_character(dsname) == TRUE,
    "Invalid dataseries. Select either 'core' or 'bmf'." =
      (dsname == "core" | dsname == "bmf")
    )
}
