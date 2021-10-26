#' Module for Importing Data
#'
#' Provides interface for uploading pairs of csv and xml files.
#'
#' After a pair of files is uploaded, the user can clicking on the \code{import}
#'  button to display the list of all variable names extracted by imported data.
#'  This documentation refers to the UI-side function of the module. For the documentation
#'  about the server-side function, got to \code{\link{importData_server}}
#'
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace str_replace_all
#'
#'
#' @param id Module's ID for namespacing
#'
#' @return \code{\link[shiny]{tagList}} that can be passed to a shinyUI function

importData_UI <- function(id) {
  ns <- NS(id)

  tagList(
    fileInput(ns("csv_file"), "Select csv file", accept = ".csv"),
    actionButton(ns("import"), "Import")
  )
}

#' Server-side Processing for Importing Data
#'
#' @param id Module's ID for namespacing
#' @param imported Reactive value containing up-to-date data imported by the user
#'  (this is shared among modules)
#' @param imported_DF Reactive value returning currently imported data frame.
#'

importData_server <- function(id, imported, imported_DF) {
  moduleServer(id, function(input, output, session) {
      data_ <- reactive({
      req(!is.null(input$csv_file$datapath))
      imported_data <- data.table::fread(input$csv_file$datapath)
      first_letter_uppercase <- function(str_) {
        first_letter <- substr(str_, 1, 1)
        substr(str_, 1, 1) <- toupper(first_letter)
        return(str_)
      }
      var_names <- names(imported_data)[-1] %>%
        str_replace_all("\\.", " ") %>%
        tolower() %>%
        map_chr(first_letter_uppercase)
      names(imported_data) <- c("ID_CODE", var_names)
      descriptive_data <- get_descriptive_binaries(imported_data)
    })

    names_ <- reactive({
      names(data_())
    })


    observeEvent(input$import, {
      DF <- data_()
      vars <- names_()
      init_DF <- data_()[, "ID_CODE", drop = FALSE]

      import_res <- list(DF = DF,
                         vars = vars,
                         init_DF = init_DF)

      imported(import_res)
      imported_DF(import_res$DF)
    })

    return(reactive(imported()))

  })
}
