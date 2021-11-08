#' Select Variables to display.
#'
#' This module allows to select which variables from imported data will be
#'  included in the crafted data frame.
#'  This documentation refers to the UI-side function of the module. For the documentation
#'  about the server-side function, got to \code{\link{selectVars_server}}
#'
#' @importFrom shinyjs reset
#'
#' @param id Modules'ID
#' @return \code{\link[shiny]{tagList}} that can be passed to a shinyUI function

selectVars_UI <- function(id) {
  ns <- NS(id)



  tagList(
    uiOutput(ns("select_ui")),
    actionButton(ns("include"), "Include selected variables"),
    actionButton(ns("clear"), "Clear"),
    hr(),
    prettyCheckbox(ns("remove_NA"), "Remove rows with NA's", shape = "round")
  )
}


#' Server-side Processing for Returning User-selected Variables
#'
#' @param id Module's ID
#' @param imported reactive value containing up-to-date data imported by the user
#'  (this is shared among modules)
#' @param selected Reactive returned by \code{selectVarsModule}, containing the
#'  array of variables the user wants to be displayed.
#' @param include_bttn Reactive value used to trigger downstream dependencies
#' @param clear_bttn Reactive value used to trigger downstream dependencies
#'

selectVars_server <- function(id,
                              imported,
                              selected,
                              include_bttn,
                              clear_bttn) {
  moduleServer(id, function(input, output, session) {


    vars <- reactive({
      if(is.null(imported()))
        NULL
      else
        unname(imported()$vars[-1])
      })

    null_selection_after_clear <- reactive({
      # Create reactive dependency on clear button
      input$clear
      # Trigger UI re-rendering of selectInput
      NULL
    })

    output$select_ui <- renderUI({
      ns <- session$ns
      selectInput(ns("select_list"),
                  "Select variables",
                  multiple = TRUE,
                  choices = vars(),
                  selectize = TRUE,
                  width = "650px",
                  selected = null_selection_after_clear())
    })

    selected_vars <- reactive({

      if(is.null(isolate(imported())))
        NULL
      else
        input$select_list
    })

    chosen_vars <- reactive(c("ID_CODE", selected_vars()))


# Observers ---------------------------------------------------------------

    observeEvent(input$include, {
      # Trigger reaction of downstream dependencies
      include_bttn(include_bttn() + 1)
      selected(input$select_list)
    })


    observeEvent(input$clear, {
                # Trigger reaction of downstream dependencies
                clear_bttn(clear_bttn() + 1)
    })


    observeEvent(input$remove_NA, {

      req(nrow(display_DF()) > 0)
      if(input$remove_NA) {
        foo <- display_DF() %>%
          remove_rows_with_NAs()
        display_DF(foo)
        rm(foo)
      } else {
        display_DF(imported_DF()[, chosen_vars()])
      }

    })
  })
}
