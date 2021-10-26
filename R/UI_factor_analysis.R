#' UI for Importing and Crafting Data
#'
#'
#'
#' @return A UI definition that can be passed to the shinyUI function


UI_factor_analysis <- function() {

  tagList(
    tabPanel("MCA", UI_MCA())
  )


}
