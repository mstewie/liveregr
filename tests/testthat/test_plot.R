context("check the plotting of the CSO Live Register data")

test_that("check the plotting of the data", {
  expect_error(plot.livereg_fit(array()))

  dat = load_livereg(use.offline.data = TRUE)
  fit = fit.livereg(dat, spline.type = "smoothing", num.knots = 10)

  expect_is(plot.livereg_fit(fit), "ggplot")
})
