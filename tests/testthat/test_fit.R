context("check fitting of Splines to the CSO Live Register data")

test_that("fitting of Spline models to the data", {
  dat = load_livereg(use.offline.data = TRUE)

  filtered_dat = filter(dat,
                        gender = "female",
                        age.group = "all",
                        start.year = 2000,
                        end.year = 2018)

  expect_error(fit.livereg(array()))

  bs_fit = fit.livereg(filtered_dat, spline.type = "b", num.knots = 10)
  expect_is(bs_fit, "livereg_fit")
  expect_true(bs_fit$seasonally.adjusted)
  expect_equal(bs_fit$num.knots, 10)
  expect_equal(bs_fit$spline.type, "b")
  expect_equal(length(bs_fit$predicted.values), 222)

  sp_fit = fit.livereg(filtered_dat, spline.type = "smoothing", num.knots = 10)
  expect_is(sp_fit, "livereg_fit")
  expect_true(sp_fit$seasonally.adjusted)
  expect_equal(sp_fit$num.knots, 10)
  expect_equal(sp_fit$spline.type, "smoothing")
  expect_equal(length(sp_fit$predicted.values), 222)

})
