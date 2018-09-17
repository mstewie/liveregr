#' @title Fits a Spline model to the CSO Live Register data
#'
#' @description Fits a Spline model to the CSO Live Register unemployment data. The number of knots can be specified so that different model fit can be assessed.
#'
#' @param object \code{livereg} S3 object
#' @param spline.type a character string of spline type choices: "b", "smoothing"
#' @param num.knots the number of Spline knots
#' @param y.col the name of the column in the data.frame to use as y
#'
#' @return The \code{livereg_fit} S3 object which contains the original data from the \code{livereg} input object as well as the fitted model and predicted value details.
#' @export
#'
#' @importFrom magrittr "%>%" "%$%" "extract2"
#' @importFrom stats "smooth.spline" "predict" "AIC" "BIC" "quantile" "lm"
#' @importFrom splines "bs"
#'
#' @examples
#' dat = load_livereg(use.offline.data = TRUE)
#' fit = fit(dat, "smoothing", num.knots = 5)
fit = function(object,
               spline.type = c("b", "smoothing"),
               num.knots = 10,
               y.col = NULL) {
  UseMethod('fit')
}

#' @export
fit.livereg = function(object,
                       spline.type = c("b", "smoothing"),
                       num.knots = 10,
                       y.col = NULL) {
  # Validate the spline.type input
  spline_type_arg = match.arg(spline.type, choices = c("b", "smoothing"))

  # Validate the number of knots
  switch (spline_type_arg,
    smoothing = {
      if (num.knots > length(object$data$time_points)) {
        stop("num.knots is greater than the number of unique time points")
      }
    },
    b = {
      if (num.knots > 10) {
        stop("num.knots greater than 10 is not supported for B-Splines")
      }
    }
  )

  # Convience data variable
  dat = object$data

  # If no column is specified then pick the first one from the data.frame
  if (is.null(y.col))
    y.col = colnames(dat)[1]

  # Create a data.frame with only the x and y columns for spline functions
  df = data.frame(x = dat$time_points %>% as.numeric,
                  y = dat %>% extract2(y.col))

  # Create the SplineModel object based on the spline type
  splineModel = SplineModelFactory(spline_type_arg)

  # Fit a spline to the data
  spline_model = splineModel$fit_model(df, num.knots)

  # Get the predicted values for the spline model
  predicted_vals = splineModel$predict(df, spline_model)

  # Model fit
  spline_model_fit = splineModel$criterion(spline_model)

  # Put all the data into a list
  out = list(
    seasonally.adjusted = object$seasonally.adjusted,
    y.col = y.col,
    data = object$data,
    spline.type = spline_type_arg,
    num.knots = num.knots,
    model = spline_model,
    model.fit = spline_model_fit,
    predicted.values = predicted_vals
  )

  # Turn the list into an S3 object
  class(out) = "livereg_fit"

  invisible(out)
}

#
# Lots of 'if', 'if-else' and 'switch' statements lead to brittle functions that are
# not easy to extend. Instead we use a little bit of object orientated programming to
# encapsulate the B-Spline and Smoothing Spline behaviour and hence the BSplineModel
# and SmoothSplineModel S3 objects.
#

#
# BSplineModel - Encapsulates the behaviour of the B-Spline model
#
BSplineModel = function() {

  me = list(
    # Fit the spline model
    fit_model = function(df, num_knots) {
      x_quantiles = df$x %>% quantile(probs = seq(0,
                                                  1,
                                                  length.out = num_knots + 1))
      bs_funs = df$x %>% bs(knots = x_quantiles)
      lm(y ~ bs_funs, data = df)
    },
    # Predict from the model
    predict = function(df, model) {
      predict(model)
    },
    criterion = function(model) {
      c(AIC = AIC(model), BIC = BIC(model))
    }
  )

  # Set the name for the class
  class(me) = append(class(me), "BSplineModel")
  return(me)
}

#
# SmoothSplineModel - Encapsulates the behaviour of the Smoothing Spline model
#
SmoothSplineModel = function() {

  me = list(
    # Fit the spline model
    fit_model = function(df, num_knots) {
      df %$% smooth.spline(x, y, nknots = num_knots)
    },
    # Predict from the model
    predict = function(df, model) {
      predict(model, df$x)$y
    },
    criterion = function(model) {
      c(crit = model$crit, pen.crit = model$pen.crit)
    }
  )

  # Set the name for the class
  class(me) = append(class(me), "SmoothSplineModel")
  return(me)
}

#
# Factory design pattern to create the correct SplineModel object.
#
# See: https://www.tutorialspoint.com/design_pattern/factory_pattern.htm
#
SplineModelFactory = function(spline_type) {

  # Create the SplineModel object based on the spline type
  switch (spline_type,
    b = {
      spline_model = BSplineModel()
    },
    smoothing = {
      spline_model = SmoothSplineModel()
    }
  )

  return(spline_model)
}

###############
# £nd Of File #
###############
