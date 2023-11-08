library(dplyr)



for (i in 1:3){

  test_data <- create_test() %>%
    dplyr::slice_head(n = 100)

  char_cols <- names(test_data %>% dplyr::select_if(is.character))
  num_cols <- names(test_data %>% dplyr::select_if(is.numeric))

  for (i in 1:5){

    group_rdm <- sample(char_cols, 4)
    var_rdm = sample(num_cols, 1)

    test_that("Summary Table Works", {
      expect_error(suppressWarnings(preview_sample(data = test_data,
                                    group_by = group_rdm,
                                    var = var_rdm,
                                    stats = c("min", "max", "median", "mean"))),
                   NA)
    })
  }
}
