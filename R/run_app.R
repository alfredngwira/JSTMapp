# run_app.R
#
#' @title A function to run the app
#' @description Runs the MBGapp Shiny web application.
#' @details The function outputs the app for the user to supply the data and analysis the data
#' @export
run_app <- function() {
  appDir <- system.file('JSTMapp', package='JSTMapp')
  if (appDir == "") {
    stop("Could not find JSTMapp. Try re-installing `JSTMapp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
