#' Run PAUDanalytics Shiny Application
#'
#' Launches the PAUDanalytics longitudinal observation application.
#'
#' @return No return value. The Shiny application is launched.
#' @export
paudshiny <- function() {
  app_dir <- system.file("shiny-app", package = "PAUDanalytics")
  if (identical(app_dir, "")) {
    stop("PAUDanalytics Shiny application files were not found.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
