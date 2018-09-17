context("check the summary of the CSO Live Register data")

test_that("check summary of the data", {
  expect_error(summary.livereg_fit(array()))

  dat = load_livereg(use.offline.data = TRUE)
  fit = fit.livereg(dat, spline.type = "smoothing", num.knots = 10)

  expect_output(summary.livereg_fit(fit), "^Basic Stats")
})
