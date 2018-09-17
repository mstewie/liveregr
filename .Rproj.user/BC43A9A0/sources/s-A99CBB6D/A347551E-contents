#' @title Plot the CSO Live Register data and fitted spline model
#'
#' @description Plots the CSO Live Register \code{livereg_fit} S3 object which contains the data and the Spline model fit.
#'
#' @param object \code{livereg_fit} S3 object
#' @param max.x.labels the maximum number of x axis labels
#' @param ... additional args
#'
#' @return ggplot of the \code{livereg_fit} S3 object.
#' @export
#'
#' @import ggplot2
#' @importFrom scales "comma" "date_breaks" "date_format"
#'
#' @examples
#' dat = load_livereg(use.offline.data = TRUE)
#' fit = fit(dat, spline.type = "smoothing", num.knots = 5)
#' plot(fit)
plot = function(object, max.x.labels = 10, ...) {
  UseMethod('plot')
}

#' @export
plot.livereg_fit = function(object, max.x.labels = 10, ...) {
  # Convenience data var
  dat = object$data

  # Convenience y var
  y.val = object$y.col

  min_year = min(dat$year)
  max_year = max(dat$year)

  # Fitted data.frame
  fitted_df = data.frame(time_points = dat$time_points,
                         pred.values = object$predicted.values)

  # Plot the data
  gg_live_reg = ggplot(dat, aes_string("time_points",
                                       y.val,
                                       color = shQuote("orig data"))) +
    #
    # Plot the live register data
    #
    # Plot the data as a line
    geom_line() +
    # Setup the theme
    theme_bw() +
    # Title attributes
    ggtitle(paste0("Live Register Data from ", min_year, " to ", max_year)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    # x-axis
    scale_x_date(labels = date_format("%Y"),  # Only print the year
                 # Dynamically adapt the date_breaks to the year range
                 date_breaks = paste0(
                   ceiling((max_year - min_year) / max.x.labels),
                   " year")) +
    xlab("Year") +
    # y-axis
    scale_y_continuous(labels = comma) +
    ylab(paste0("Age Group = ", y.val)) +
    # Legend setup
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.background = element_rect(colour = "black", size = 0.25),
          legend.title = element_blank()) +
    #
    # Plot the spline
    #
    # Overlay the fitted model on the data plot
    geom_line(data = fitted_df, aes_string("time_points",
                                           "pred.values",
                                           color = shQuote("fitted model")))

  # Print the plot
  gg_live_reg
}

###############
# £nd Of File #
###############
