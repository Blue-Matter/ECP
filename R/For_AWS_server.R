#' Exceptional Circumstances Protocols (ECP)
#'
#' Launches the ECP Shiny App.
#'
#' @param ... Arguments to \link[shiny]{runApp}.
#' @details \code{ECP} opens up the App in the user browser.
#'
#' @examples
#' ECP(launch.browser = TRUE)
#'
#' @export
ECP <- function(...) shiny::runApp(system.file("shiny_apps/ECP", package = "ECP"), ...)
