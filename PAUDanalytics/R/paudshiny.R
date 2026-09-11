#' Run PAUDanalytics Shiny Application
#'
#' Launches the longitudinal PAUD observation analytics dashboard.
#'
#' @return A running Shiny application.
#' @export
paudshiny <- function() {
  app_dir <- system.file("shiny-app", package = "PAUDanalytics")
  if (app_dir == "") stop("Shiny application files were not found.")
  shiny::runApp(app_dir, display.mode = "normal")
}
