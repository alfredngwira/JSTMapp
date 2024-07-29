#' Runs the Shiny web application.
#' @export
run_app <- function() {
  shiny::runApp(system.file('JSTMapp', package='JSTMapp'))
}
