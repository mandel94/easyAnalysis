#' UI for Transforming the Data
#'
#'
#' @param transform_id ID of \code{transformDataModule} module
#'
#' @return A UI definition that can be passed to the shinyUI function

UI_transform <- function(transform_id = "transform",
                         display_id = "display_transform") {

    ui <- fluidPage(
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel("Survey Data",
                     transformSurvey_UI("transform_survey")),
            tabPanel("Numerical Data",
                     transformNumerical_UI("transform_numerical"))
          )
        ),
        mainPanel(
          displayTransform_UI(display_id)
        )
      )
    )

    ui
}
