library(aws.s3)
library(arrow)
library(dplyr)

# Load data from S3
# Note: The accompanying data dictionary located here: https://nccsdata.s3.us-east-1.amazonaws.com/geocoding/bmf/2026_03/merged/bmf_2026_03_geocoding_data_dictionary.csv
# This is a public S3 bucket
bmf <- arrow::open_dataset("s3://nccsdata/geocoding/bmf/2026_03/merged/bmf_2026_03_geocoded.parquet")

# Filtered BMF: In this case, need to filter for pennsylvania
bmf_pa <- bmf |>
  dplyr::filter(org_addr_state == "PA") |>
  dplyr::collect()

# Filter for select counties: Lackawanna, Luzerne, and Wayne counties
bmf_pa_counties <- bmf_pa |>
  dplyr::filter(
    geo_county == "Lackawanna County" |
      geo_county == "Luzerne County" |
      geo_county == "Wayne County"
  )


# Total Tax Exempt Organizations
# County-level and statewide counts of total tax-exempt organizations
# for Lackawanna, Luzerne, and Wayne counties, and Pennsylvania overall in most recent year (2026).

bmf_pa_count_summary <- bmf_pa |>
  dplyr::select(ein) %>%
  dplyr::summarise(n = n())

bmf_pa_counties_count_summary <- bmf_pa_counties |>
  dplyr::select(ein, geo_county) %>%
  dplyr::group_by(geo_county) %>%
  dplyr::summarise(n = n())

# Non-Profits by Activity Type (by County and Pennsylvania)
# Counts of nonprofit organizations broken down by activity type for
# Lackawanna, Luzerne, and Wayne counties, and Pennsylvania overall.
# Activity types: Arts/culture/humanities, Education, Environment/animals,
# Health care, Human services, Religion, Mutual benefit, etc.

bmf_pa_count_summary <- bmf_pa |>
  dplyr::select(ein, nteev2_subsector_definition) %>%
  dplyr::group_by(nteev2_subsector_definition) %>%
  dplyr::summarise(n = n())

bmf_pa_counties_count_summary <- bmf_pa_counties |>
  dplyr::select(ein, geo_county, nteev2_subsector_definition) %>%
  dplyr::group_by(geo_county, nteev2_subsector_definition) %>%
  dplyr::summarise(n = n())

# note: UNDEFINED major group should be changed to lowercase and means the field was blank. unknown means an invalid code such as Z99 was entered as a placeholder for an unknown code
# Note:
# ART - Arts, Culture, and Humanities
# EDU - Education
# ENV - Environment and Animals
# HEL - Health
# HMS - Human Services
# IFA - International, Foreign Affairs
# PSB - Public, Societal Benefit
# REL - Religion Related
# MMB - Mutual/Membership Benefit
# UNU - Unknown, Unclassified
# UNI - Universities
# HOS - Hospitals

# Exempt Organizations by Category: 2024
# Counts of exempt organizations by IRS subsection code for
# Lackawanna, Luzerne, and Wayne counties, and Pennsylvania overall.
# Year: 2026 - most recent
# Categories: 501(c)3, 501(c)4, 501(c)5, 501(c)6, 501(c)7,
# 501(c)19, etc.

bmf_pa_count_summary <- bmf_pa |>
  dplyr::select(ein, exempt_organization_type) %>%
  dplyr::summarise(n = n())

bmf_pa_counties_count_summary <- bmf_pa_counties |>
  dplyr::select(ein, geo_county, exempt_organization_type) %>%
  dplyr::group_by(geo_county, exempt_organization_type) %>%
  dplyr::summarise(n = n())
