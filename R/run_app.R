#' Launch the dataQI Shiny application
#'
#' Opens the interactive data quality inspection dashboard in the default
#' browser.
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#' @return This function is called for its side effect of launching a Shiny
#'   application and does not return a meaningful value.
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
run_app <- function(...) {
  app_dir <- system.file("app", package = "dataQI")
  if (app_dir == "") {
    stop("Could not find the app directory. Try re-installing 'dataQI'.")
  }
  shiny::runApp(app_dir, ...)
}
