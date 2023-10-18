#' @title Function to summarize NCCS data.
#'
#' @description This function computes summary statistics for data pulled with
#' get_data()
#'
#' @param data data.frame or data.table. In-memory dataset to summarize
#' @param group_by character vector. Vector of columns for dplyr::group_by()
#' @param var character scalar. Column to calculate summary statistics with
#' @param stats character vector. Vector of summary statistics to compute with
#' dplyr::summarise(). Available options are count, min, max, median and mean
#' @param geo.state character vector. Filter query by state abbreviations e.g.
#' "NY", "CA". Default == NULL includes all states.
#' @param geo.city character vector. City names for filtering e.g. "Chicago",
#' "montgomery". Case insensitive
#' @param geo.county character vector. County names for filtering e.g.
#' "cullman", "dale". Case insensitive.
#' @param geo.region character vector. Regions for filtering e.g. "South",
#' "Midwest" based on census region classifications.
#' @param ntee character vector. Vector of user inputs. The user inputs are
#' progressively filtered until group, code and orgtypes are sorted into
#' separate vectors.
#' @param ntee.group character vector. Specific Industry Group codes submitted
#' by user
#' @param ntee.code character vector. Specific level 2-4 codes (Industry,
#' Division, Subdivision) submitted by user.
#' @param ntee.orgtype character vector. Specific level 5 codes (Organization
#' Type) submitted by user.
#'
#' @returns dataframe with summary statistics computed for each group
#'
#' @examples
#' \dontrun{
#' core <- get_data(dsname = "core",
#'                  time = "2005")
#' preview_sample(data = core,
#'                group_by = c("NTEECC", "STATE"),
#'                var = c("TOTREV"),
#'                stats = c("count", "mean", "max"))
#' }
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr all_of
#' @importFrom dplyr across
#' @importFrom dplyr select
#' @importFrom stats median
#' @importFrom data.table as.data.table
#'
#' @export

preview_sample <- function(data,
                           group_by,
                           var,
                           stats,
                           ntee = NULL,
                           ntee.group = NULL,
                           ntee.code = NULL,
                           ntee.orgtype = NULL,
                           geo.state = NULL,
                           geo.city = NULL,
                           geo.region = NULL,
                           geo.county = NULL){

  # Validate entries
  validate_preview(df_cols = colnames(data),
                   group_by = group_by,
                   var = var,
                   stats = stats)

  # Create filters
  filter_ls <- list(nteecc_matches = nteecc_map(ntee.user = ntee,
                                                ntee.group = ntee.group,
                                                ntee.code = ntee.code,
                                                ntee.orgtype = ntee.orgtype),
                    state_matches = toupper(geo.state),
                    city_matches = toupper(geo.city),
                    county_fips_matches = fips_map(geo.region = firstupper(geo.region),
                                                   geo.county = geo.county))

  data <- filter_data(dt = data.table::as.data.table(data),
                      filters = filter_ls)

  preview <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
    dplyr::summarise(count = n(),
                     min = min(!!sym(var)),
                     mean = mean(!!sym(var)),
                     median = stats::median(!!sym(var)),
                     max = max(!!sym(var))) %>%
    dplyr::select(dplyr::all_of(c(group_by, stats)))

  return(preview)

}
