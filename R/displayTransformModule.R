#' Module for Displaying Crafted Data Frame (on Transform Navbar Page)
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

displayTransform_UI <- function(id) {
  ns <- NS(id)

  shinycssloaders::withSpinner(
    tableOutput(ns("table"))
  )
}



displayTransform_server <- function(id,
                                    display_DF) {
  moduleServer(id, function(input, output, session) {

    output$table <- renderTable({
      browser
      if(is.data.frame(display_DF()))
        display_DF()
      else
        data.frame()
    })

  })
}
