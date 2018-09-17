#' @title Launch the CSO Umemployment Live Register Shiny App
#'
#' @description This function launches a Shiny application which uses the functions exported by the \code{liveregr} package.
#'
#' @return This method does not return anything. It just launches the Shiny app.
#' @export
#'
#' @importFrom shiny "runApp"
#'
#' @examples
#' \dontrun{
#' shiny_livereg()
#' }
shiny_livereg = function() {
  # Set the appDir to point to the location of the ui.R and server.R files
  appDir = system.file("shiny_app", package = "liveregr")
  if (appDir == "") {
    stop("Could not find the shiny app. Try re-installing `liveregr`.",
         call. = FALSE)
  }

  # Launch the Shiny application
  shiny::runApp(appDir, display.mode = "normal")
}

###############
# £nd Of File #
###############
