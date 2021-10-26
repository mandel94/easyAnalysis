#' UI for Importing and Crafting Data
#'
#'
#' @param import_id ID of \code{importDataModule}
#' @param select_id ID of \code{selectVarsModule}
#' @param display_id ID of \code{displayDataModule} module
#'
#' @return A UI definition that can be passed to the shinyUI function

UI_import <- function(import_id = "import",
                      select_id = "select",
                      display_id = "display_import") {
  fluidPage(sidebarLayout(
    sidebarPanel(importData_UI(import_id),
                 selectVars_UI(select_id)),
    mainPanel(displayImport_UI(display_id))
  ))

}
