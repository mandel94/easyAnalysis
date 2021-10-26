#' Module for Transforming Survey Data
#'
#' This module can be used for creating synthetic variables.



#' UI function
#' @param id Module's ID for namespacing
#'
#' @return \code{\link[shiny]{tagList}} that can be passed to a shinyUI function
#'
#' @importFrom shinyWidgets radioGroupButtons prettyCheckbox actionBttn

transformSurvey_UI <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("vars_pool")),
    radioGroupButtons(
      ns("logical_op"),
      "Choose a logical operator",
      choices = c("AND", "OR", "NOT"),
      status = "default",
      size = "normal",
      direction = "horizontal",
      justified = FALSE,
      individual = FALSE,
      checkIcon = list(),
      width = "100%",
      disabled = FALSE
    ),
    textInput(
      ns("new_name"),
      "Name the new variable",
      value = "new_variable",
      width = "100%",
      placeholder = "new_variable"
    ),
    prettyCheckbox(
      ns("overwrite"),
      "Overwrite used variables",
      value = TRUE,
      status = "primary",
      icon = icon("check"),
      shape = "curve",
      fill = FALSE
    ),
    actionBttn(ns("update"),
               "Update",
               style = "pill",
               color = "primary"),
    hr(),
    prettyCheckbox(ns("filter_columns"),
                   label = "Apply variance filter to columns"),
    helpText("This filter will keep only those columns whose variance exceeds
             some user given threshold. This can be useful if there are some
             variables with low variance (i.e., a very small % of yes or no).
             These variables tend to exert an excessive influence on factorial
             analysis results, hindering the interpretability of resulting
             factors."),
    uiOutput(ns("var_threshold_slider"))
  )
}



#' Server-side Processing for Applying Transformations to Current Data Frame
#'
#' @param id Module's ID for namespacing
#' @param display_DF Reactive value returning the data frame to be displayed,
#'  updated with current selected variables and applied transformations
#' @param imported_DF Reactive value returning the currently imported data
#'  frame.
#' @param displayed_vars Reactive value returning currently displayed variables

transformSurvey_server <- function(id,
                                   imported_DF,
                                   display_DF,
                                   displayed_vars) {
  moduleServer(id, function(input, output, session) {



# Initialize Values  ------------------------------------------------------



# UI-Rendering ------------------------------------------------------------

    output$vars_pool <- renderUI({
      ns <- session$ns

      selectInput(
        ns("selected"),
        "Pool of variables",
        multiple = TRUE,
        choices = displayed_vars()[-1],
        selectize = TRUE,
        width = "100%"
      )
    })

    output$var_threshold_slider <- renderUI({
      ns <- session$ns
      if(input$filter_columns)
        tagList(
          tags$p("Choose a minimum variance threshold",
                 class = "ui-control-title"),
          sliderInput(ns("var_threshold"),
                      label = "",
                      min = 0,
                      max = 1,
                      value = 0)
        )
      else
        NULL
    })


# Observers ---------------------------------------------------------------


    observeEvent(input$update, {
      validate(
        need(is.data.frame(display_DF()),
             message = "No data uploaded yet!"),
        need(validate_update(input$selected, input$logical_op),
             message = "Number of selected variables incompatible
                        with selected operator"
        )
      )


      # Get variables resulting from transformation
      get_transform_out <- function(DF) {
        transform_(DF,
                   input$selected,
                   input$logical_op,
                   input$new_name,
                   input$overwrite)
      }


      # Update the dataframe to be displayed
      out <- get_mutated_DF(display_DF(),
                              get_transform_out(display_DF()),
                              input$selected)
      # out_2 <- get_mutated_DF(imported_DF(),
      #                         get_transform_out(imported_DF()),
      #                         input$selected)
      # imported_DF(out_2)
      display_DF(out)

      displayed_vars(names(display_DF()))

    })

    observeEvent(input$var_threshold, {
      out <- minimal_variance_filter(display_DF(),
                                     threshold = input$var_threshold)
      display_DF(out)
    })


  })
}
