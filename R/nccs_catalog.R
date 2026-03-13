#' List Valid Filter Values
#'
#' Returns the set of valid values for filters used in [nccs_read()].
#' No network access is needed.
#'
#' @param field One of `"ntee_subsector"`, `"exempt_org_type"`, or `"state"`.
#'
#' @return A character vector of valid values for the specified field.
#'
#' @examples
#' nccs_catalog("ntee_subsector")
#' nccs_catalog("state")
#' nccs_catalog("exempt_org_type")
#'
#' @export
nccs_catalog <- function(field = c("ntee_subsector", "exempt_org_type", "state")) {
  field <- match.arg(field)

  switch(field,
    ntee_subsector = c(
      "ART", "EDU", "ENV", "HEL", "HMS", "HOS",
      "IFA", "MMB", "PSB", "REL", "UNI", "UNU"
    ),
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
    )
  )
}
