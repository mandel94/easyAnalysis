#'This module defines dependencies to css stylesheets
#'
#' @importFrom htmltools htmlDependency
#' @import shiny
#'
stylesheet_dep <- function() {
  htmlDependency(
  name = "css style",
  version = "1.0",
  src = "www/",
  stylesheet = "stylesheet.css"
  )
}




