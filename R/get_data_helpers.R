#' @export
validate_get_data <- function(dsname = NULL,
                              time = "current",
                              scope.orgtype = "NONPROFIT",
                              scope.formtype = "PC",
                              ntee = NULL,
                              geo = NULL){

  stopifnot(

    "Invalid datatype. dsname must be a string." =
      is_scalar_character(dsname) == TRUE,

    "Invalid dataseries. Select either 'core' or 'bmf'." =
      (dsname == "core" | dsname == "bmf"),

    "Invalid datatype. time must be a character vector." =
      is_character(time) == TRUE,

    "Invalid date range specified. Choose years from 1989-2022." =
      all(as.numeric(time) %in% c(seq(1989, 2022), "current")),

    "Invalid datatype. scope.orgtype must be a string." =
      is_scalar_character(scope.orgtype) == TRUE,

    "Invalid organisation type. Select 'CHARITIES' for charities (501C3-PC), 'PRIVFOUND' for private foundations (501C3-PF) and 'NONPROFIT' for all nonprofits (501CE) " =
      (scope.orgtype == "CHARITIES" |
       scope.orgtype == "PRIVFOUND" |
       scope.orgtype == "NONPROFIT"),

    "Invalid datatype. scope.formtype must be a string." =
      is_scalar_character(scope.formtype) == TRUE,

    "Invalid formtype. Select 'PC'(nonprofits that file the full version), 'EZ'(nonprofits that file 990EZs only), 'PZ'(nonprofits that file both PC and EZ), or 'PF'(private foundations)" =
      (scope.formtype == "PC" |
       scope.formtype == "EZ" |
       scope.formtype == "PZ" |
       scope.formtype == "PF")
    )
}
