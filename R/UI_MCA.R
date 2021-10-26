#' UI for Importing and Crafting Data
#'
#'
#' @return A UI definition that can be passed to the shinyUI function

UI_MCA <- function() {
  fluidPage(sidebarLayout(sidebarPanel(controlMCA_UI("MCA_control")),
                          mainPanel(tagList(
                            plotMCA_UI("MCA_output")
                          ))))

}
