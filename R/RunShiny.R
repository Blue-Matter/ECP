#' Run a Shiny Application
#'
#' \code{Shiny} runs one of the Shiny Applications that are included in the package
#'
#' @param app The name of the Shiny application to run.
#' @references Modified from Deal Attali's code: \url{http://deanattali.com/2015/04/21/r-package-shiny-app/}
#' @importFrom utils install.packages installed.packages
#' @export
ECPapp2 <- function(){
  app="ECP"
  appDir <- system.file("shiny_apps", app, package = "ECP")
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)
}
