context("check that the CSO Live Register data loads ok")

test_that("load_livereg loads valid data", {
  expect_is(load_livereg(use.offline.data = TRUE), "livereg")
  expect_error(load_livereg(use.data = FALSE))
  expect_equal(length(load_livereg(use.offline.data = TRUE)$data$time_points), 618)
})
