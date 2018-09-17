context("check filtering of the CSO Live Register data")

test_that("filter the data correctly", {
  dat = load_livereg(use.offline.data = TRUE)

  expect_error(filter.livereg(list()))

  filter_year = filter.livereg(dat, start.year = 2000, end.year = 2010)
  expect_is(filter_year, "livereg")
  expect_equal(min(filter_year$data$year), 2000)
  expect_equal(max(filter_year$data$year), 2010)

  gender_filter = filter.livereg(dat, gender = "female")
  expect_match(names(gender_filter$data[1]), "^female")

  age_group_filter = filter.livereg(dat, age.group = "under_25")
  expect_match(names(age_group_filter$data[1]), "under_25$")
})
