test_that("get_data() NTEE filtering works", {

  # Download random core dataset
  test_data <- create_test()

  # Create filters
  ntee2_codes <- ntee_df$ntee2.code
  groups <- unique(substr(ntee2_codes, 1, 3))
  codes <- unique(substr(ntee2_codes, 5, 7))
  orgs <- unique(substr(ntee2_codes, 9, 10))

  for (i in 1:10){
    nteecc_codes <- nteecc_map(ntee.user = sample(ntee2_codes, 5),
                               ntee.group = sample(groups, 2),
                               ntee.code = sample(codes, 3),
                               ntee.orgtype = sample(orgs, 2))

    filter_ls <- list(nteecc_matches = nteecc_codes,
                      state_matches = NULL,
                      city_matches = NULL,
                      county_fips_matches = NULL)

    # Filter data if specified
    test_data <- filter_data(test_data,
                             filters = filter_ls)

    expect_contains(unique(nteecc_codes), unique(test_data$NTEECC))
  }
})

test_that("get_data() state filtering works", {

  test_data <- create_test()

  # Create filters
  states <- na.omit(unique(cbsa_df$state.census.abbr))

  for (i in 1:10){

    # State filter
    states_rdm <- sample(states, 5)
    filter_ls <- list(nteecc_matches = NULL,
                      state_matches = states_rdm,
                      city_matches = NULL,
                      county_fips_matches = NULL)

    # Filter data if specified
    test_filtered <- filter_data(test_data,
                                 filters = filter_ls)

    expect_equal(sort(states_rdm), sort(unique(test_filtered$STATE)))


  }
})

test_that("get_data() region filtering works", {

  test_data <- create_test()

  # Create filters
  regions <- unique(tract_dat$region.census.main)

  for (i in 1:10){

    # Region filter
    region_rdm <- sample(regions, 2)
    county_fips <- map_countyfips(geo.region = region_rdm,
                                  geo.county = NULL)

    filter_ls <- list(nteecc_matches = NULL,
                      state_matches = NULL,
                      city_matches = NULL,
                      county_fips_matches = county_fips)

    # Filter data if specified
    test_filtered <- filter_data(test_data,
                                 filters = filter_ls)

    expect_contains(sort(unique(county_fips)), sort(unique(test_filtered$FIPS)))


  }
})

test_that("get_data() county filtering works", {

  test_data <- create_test()

  # Create filters
  counties <- unique(cbsa_df$census.county.name)

  for (i in 1:10){

    # Region filter
    county_rdm <- sample(counties, 2)
    county_fips <- map_countyfips(geo.region = NULL,
                                  geo.county = county_rdm)

    filter_ls <- list(nteecc_matches = NULL,
                      state_matches = NULL,
                      city_matches = NULL,
                      county_fips_matches = county_fips)

    # Filter data if specified
    test_filtered <- filter_data(test_data,
                                 filters = filter_ls)

    expect_contains(sort(unique(county_fips)), sort(unique(test_filtered$FIPS)))


  }
})
