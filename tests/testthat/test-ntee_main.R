test_that("ntee_preview() filters Level 1 correctly", {
  level1 <- unique(ntee_df$broad.category)
  for (i in level1){
  expect_match(ntee_preview(ntee = i), i)
  expect_match(ntee_preview(ntee.group = i), i)
  }
})

test_that("ntee_preview() filters Level 2 correctly", {
  level2 <- paste0(unique(substr(ntee_df$ntee2.code, 5, 5)), "xx")
  for (lvl in level2){
    rgxp <- paste0(unique(substr(lvl, 1, 1)), "[062589347][0-9A-F]")
    expect_match(ntee_preview(ntee = lvl), rgxp)
    expect_match(ntee_preview(ntee.code = lvl), rgxp)
  }
})

test_that("ntee_preview() filters Levels 2-3 correctly", {
  level23 <- paste0(unique(substr(ntee_df$ntee2.code, 5, 6)), "x")
  for (lvl in level23){
    rgxp <- paste0(unique(substr(lvl, 1, 2)), "[0-9A-F]")
    expect_match(ntee_preview(ntee = lvl), rgxp)
    expect_match(ntee_preview(ntee.code = lvl), rgxp)
  }
})

test_that("ntee_preview() filters Levels 2-4 correctly", {
  level24 <- unique(substr(ntee_df$ntee2.code, 5, 7))
  for (lvl in level24){
    expect_match(ntee_preview(ntee = lvl), lvl)
    expect_match(ntee_preview(ntee.code = lvl), lvl)
  }
})


test_that("ntee_preview() filters Level 5 correctly", {
  level5 <- unique(ntee_df$type.org)
  for (i in level5){
    expect_match(ntee_preview(ntee = i), i)
    expect_match(ntee_preview(ntee.orgtype = i), i)
  }
})

test_that("ntee_preview() filters Levels 2-5 correctly", {
  level25 <- unique(ntee_df$ntee2.code)
  for (i in level25){
    expect_match(ntee_preview(ntee = i), i)
  }
})
