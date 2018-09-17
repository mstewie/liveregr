#' @title Summary of the CSO Live Register data
#'
#' @description Summary of the dataset statistics and Spline model of the CSO Live Register unemployment data.
#'
#' @param object \code{livereg_fit} S3 object
#'
#' @return Summary of the dataset statistics and Spline model of the \code{livereg_fit} object.
#' @export
#'
#' @importFrom magrittr "%>%" "subtract"
#'
#' @examples
#' dat = load_livereg(use.offline.data = TRUE)
#' summary(dat)
summary = function(object) {
  UseMethod('summary')
}

#' @export
summary.livereg_fit = function(object) {
  # Basic Info
  cat("Basic Stats\n")
  cat("-----------\n")
  print(c(seasonally.adjusted = object$seasonally.adjusted,
        y.col = object$y.col))
  cat("\n")

  # Convenience data var
  dat = object$data

  # Start and End Year
  print(c(start.year = dat$year %>% min, end.year = dat$year %>% max))
  cat("\n")

  # Basic statistics on the selected y.col
  dat_tbl = summary(dat)
  year_col = dat %>% colnames %>% length %>% subtract(2)
  print(dat_tbl[, c(1, year_col)])

  # Model stats
  cat("Model Stats\n")
  cat("-----------\n")
  print(c(spline.type = object$spline.type,
        num.knots = object$num.knots))
  cat("\n")
  cat("Model Fit\n")
  cat("---------\n")
  print(object$model.fit)

  # The summary method didn't enrich the livereg_fit object therefore we don't
  # return anything
  invisible()
}

###############
# £nd Of File #
###############

