#' List Valid Filter Values
#'
#' Returns the set of valid values for filters used in [nccs_read()], or — with
#' `labels = TRUE` — a tibble of codes alongside their human-readable
#' definitions sourced from bundled BMF lookup tables. No network access.
#'
#' @param field One of `"ntee_subsector"`, `"ntee_major_group"`,
#'   `"exempt_org_type"`, `"state"`, `"subsection_code"`,
#'   `"foundation_code"`, or `"affiliation_code"`. The last three are
#'   decoder fields backed by the bundled BMF lookups; they are not
#'   currently accepted by `nccs_read()` but are useful for joining
#'   external data.
#' @param labels Logical. If `FALSE` (default), returns a character vector of
#'   valid codes — preserves the legacy behavior. If `TRUE`, returns a tibble
#'   with at least a `code` and `description` column, sourced from the
#'   internal `.nccs_lookups` bundle (see `data-raw/build_lookups.R`). For
#'   `"state"`, the tibble pairs USPS abbreviations with state names; for
#'   `"exempt_org_type"`, codes come from `subsection_classification_code`.
#'
#' @return A character vector (when `labels = FALSE`) or a tibble (when
#'   `labels = TRUE`).
#'
#' @examples
#' nccs_catalog("ntee_subsector")
#' nccs_catalog("ntee_subsector", labels = TRUE)
#' nccs_catalog("state", labels = TRUE)
#' nccs_catalog("foundation_code", labels = TRUE)
#'
#' @export
nccs_catalog <- function(field = c("ntee_subsector", "ntee_major_group",
                                   "exempt_org_type", "state",
                                   "subsection_code", "foundation_code",
                                   "affiliation_code"),
                         labels = FALSE) {
  field <- match.arg(field)
  if (!is.logical(labels) || length(labels) != 1L || is.na(labels)) {
    stop("`labels` must be TRUE or FALSE.", call. = FALSE)
  }

  if (labels) return(.nccs_catalog_labels(field))
  .nccs_catalog_codes(field)
}

#' @noRd
.nccs_catalog_codes <- function(field) {
  switch(field,
    ntee_subsector = c(
      "ART", "EDU", "ENV", "HEL", "HMS", "HOS",
      "IFA", "MMB", "PSB", "REL", "UNI", "UNU"
    ),
    ntee_major_group = LETTERS,
    state = c(
      datasets::state.abb, "DC", "PR", "GU", "VI", "AS", "MP"
    ),
    exempt_org_type = c(
      "501(c)(1) - Corporations Organized Under Act of Congress",
      "501(c)(2) - Title Holding Corporation For Exempt Organization",
      "501(c)(3) - Religious, Educational, Charitable, etc.",
      "501(c)(4) - Civic Leagues, Social Welfare Organizations",
      "501(c)(5) - Labor, Agricultural, Horticultural Organizations",
      "501(c)(6) - Business Leagues, Chambers of Commerce",
      "501(c)(7) - Social and Recreational Clubs",
      "501(c)(8) - Fraternal Beneficiary Societies and Associations",
      "501(c)(9) - Voluntary Employee Beneficiary Associations",
      "501(c)(10) - Domestic Fraternal Societies and Associations",
      "501(c)(11) - Teachers Retirement Fund Associations",
      "501(c)(12) - Benevolent Life Insurance Associations",
      "501(c)(13) - Cemetery Companies",
      "501(c)(14) - State-Chartered Credit Unions",
      "501(c)(15) - Mutual Insurance Companies or Associations",
      "501(c)(16) - Cooperative Organizations to Finance Crop Operations",
      "501(c)(17) - Supplemental Unemployment Benefit Trusts",
      "501(c)(18) - Employee Funded Pension Trusts",
      "501(c)(19) - Post or Organization of Past or Present Members of the Armed Forces",
      "501(c)(20) - Group Legal Services Plan Organizations",
      "501(c)(21) - Black Lung Benefit Trusts",
      "501(c)(22) - Withdrawal Liability Payment Fund",
      "501(c)(23) - Veterans Organization",
      "501(c)(24) - Section 4049 ERISA Trusts",
      "501(c)(25) - Title Holding Corporations or Trusts",
      "501(c)(26) - State-Sponsored High Risk Health Coverage Organizations",
      "501(c)(27) - State-Sponsored Workers Compensation Reinsurance Organizations",
      "501(c)(28) - National Railroad Retirement Investment Trust",
      "501(c)(29) - CO-OP Health Insurance Issuers",
      "501(c)(40) - Apostolic and Religious Organizations",
      "501(d) - Religious and Apostolic Organizations",
      "501(e) - Cooperative Hospital Service Organizations",
      "501(f) - Cooperative Service Organizations of Operating Educational Organizations"
    ),
    subsection_code  = as.character(unique(.nccs_lookups$subsection_classification_code$subsection_code)),
    foundation_code  = as.character(.nccs_lookups$foundation_code$foundation_code),
    affiliation_code = as.character(.nccs_lookups$affiliation_code$affiliation_code)
  )
}

#' @noRd
.nccs_catalog_labels <- function(field) {
  switch(field,
    ntee_subsector = {
      tbl <- .nccs_lookups$nteev2_subsector
      dplyr::tibble(
        code        = as.character(tbl$nteev2_subsector),
        description = as.character(tbl$nteev2_subsector_definition)
      )
    },
    ntee_major_group = dplyr::tibble(
      code = LETTERS,
      description = c(
        "Arts, Culture, and Humanities",
        "Education",
        "Environment",
        "Animal-Related",
        "Health Care",
        "Mental Health and Crisis Intervention",
        "Voluntary Health Associations and Medical Disciplines",
        "Medical Research",
        "Crime and Legal-Related",
        "Employment",
        "Food, Agriculture, and Nutrition",
        "Housing and Shelter",
        "Public Safety, Disaster Preparedness, and Relief",
        "Recreation and Sports",
        "Youth Development",
        "Human Services",
        "International, Foreign Affairs, and National Security",
        "Civil Rights, Social Action, and Advocacy",
        "Community Improvement and Capacity Building",
        "Philanthropy, Voluntarism, and Grantmaking Foundations",
        "Science and Technology",
        "Social Science",
        "Public and Societal Benefit",
        "Religion-Related",
        "Mutual and Membership Benefit",
        "Unknown"
      )
    ),
    state = {
      abbr <- c(datasets::state.abb, "DC", "PR", "GU", "VI", "AS", "MP")
      name <- c(datasets::state.name,
                "District of Columbia", "Puerto Rico", "Guam",
                "U.S. Virgin Islands", "American Samoa",
                "Northern Mariana Islands")
      dplyr::tibble(code = abbr, description = name)
    },
    exempt_org_type = {
      tbl <- .nccs_lookups$subsection_classification_code
      uniq <- !duplicated(tbl$subsection_code)
      dplyr::tibble(
        code        = as.character(tbl$subsection_code[uniq]),
        description = as.character(tbl$exempt_organization_type[uniq])
      )
    },
    subsection_code = {
      tbl <- .nccs_lookups$subsection_classification_code
      dplyr::tibble(
        code                       = as.character(tbl$subsection_code),
        classification_code        = as.character(tbl$classification_code),
        description                = as.character(tbl$classification_description),
        exempt_organization_type   = as.character(tbl$exempt_organization_type)
      )
    },
    foundation_code = {
      tbl <- .nccs_lookups$foundation_code
      dplyr::tibble(
        code        = as.character(tbl$foundation_code),
        description = as.character(tbl$foundation_code_definition)
      )
    },
    affiliation_code = {
      tbl <- .nccs_lookups$affiliation_code
      dplyr::tibble(
        code        = as.character(tbl$affiliation_code),
        description = as.character(tbl$affiliation_code_definition)
      )
    }
  )
}
