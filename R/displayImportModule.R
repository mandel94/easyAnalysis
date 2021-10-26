#' Module for Displaying Crafted Data Frame (on Import Navbar Page)
#'
#' This is module will be used to display the data frame crafted by the user.
#'  In this circumstance, crafting is the result of two phases:
#'  \describe{
#'      \item{select}{the selection of variables from the list resulting from imported data}
#'      \item{transform}{the transformations applied by the user to the original data}
#'  }
#'  This documentation refers to the UI-side function of the module. For the documentation
#'  about the server-side function, got to \code{\link{displayData_server}}
#'
#' @import dplyr
#'
#'
#' @param id Module's ID for namespacing
#' @return \code{\link[shiny]{tagList}} that can be passed to a shinyUI function

displayImport_UI <- function(id) {
  ns <- NS(id)

  shinycssloaders::withSpinner(
    tableOutput(ns("table"))
  )
}



#' Server-side Processing for Displaying the Data Frame
#'
#'
#' @param id Module's ID for namespacing
#' @param imported Reactive resulting from \code{importDataModule}, containing,
#'  among other thing, the data frame as originally imported.
#' @param selected Reactive returned by \code{selectVarsModule}, containing the
#'  array of variables the user wants to be displayed.
#' @param include_bttn Reactive value set upstream to trigger data frame displaying
#' @param display_DF Reactive values returning the data frame to be displayed,
#'  updated with current selected variables and applied transformations
#' @param clear_bttn Reactive value triggered upstream for displaying DF without
#'  selected variables
#' @param displayed_vars Reactive value returning currently displayed variables
#'

displayImport_server <- function(id,
                                 imported,
                                 selected,
                                 include_bttn,
                                 clear_bttn,
                                 display_DF,
                                 displayed_vars) {
  moduleServer(id, function(input, output, session) {
    # Data frame as originally imported
    DF <- reactive({
      imported_DF <- imported()$DF

      if (is.data.frame(imported_DF))
        names(imported_DF) <- imported()$vars

      imported_DF

    })

    # Variables selected by the
    chosen_vars <- reactive(c("ID_CODE", selected()))

    # Data frame to be displayed
    display_DF(isolate(DF()))


    observeEvent(include_bttn(), {
      validate(need(is.data.frame(DF()), "You have to import some data first"))

      updated_DF <- select(DF(),
                           all_of(chosen_vars()))

      x <- tryCatch({
        duplicate_idx <-
          which(names(updated_DF)[-1] %in% names(display_DF())) + 1
        if(length(duplicate_idx) > 0) {
          updated_DF <- updated_DF[, -duplicate_idx, drop=FALSE]
          merge(display_DF(), updated_DF, by = "ID_CODE")
        } else {
          merge(display_DF(), updated_DF, by = "ID_CODE")
        }
      },
      error = function(e) {
        updated_DF
      })

      display_DF(x)

      displayed_vars(names(display_DF()))

      rm(updated_DF)

    })


    observeEvent(clear_bttn(), {
      validate(need(is.data.frame(DF()), "You have to import some data first"))

      updated_DF <- select(DF(),
                           ID_CODE)

      display_DF(updated_DF)

      displayed_vars(names(display_DF()))

    })


# UI-rendering ------------------------------------------------------------


    output$table <- renderTable({
      if (is.data.frame(display_DF()))
        display_DF()
      else
        data.frame()

    })


  })
}
