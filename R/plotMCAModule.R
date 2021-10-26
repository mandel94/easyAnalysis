#' Module for plotting results of MCA
#'
#'
#' @param id Modules's id


plotMCA_UI <- function(id) {
  ns <- NS(id)

  tagList(plotOutput(ns("eigen_plot")),
          hr(),
          plotOutput(ns("MCA_plot")),
          uiOutput(ns("render_order_by")),
         )
}

#' Server-side processing
#'
#'
#' @param id Modules's id
#' @param plot_DF Reactive value containing the dataframe to be plotted.
#' @param eigenvalues Reactive value containing the eigenvalues of MCA factors.
#' @param active_entity Reactive value, returning the active entity (individual,
#'  variable)
#' @param active_dims Reactive value, returning currently selected dimensions
#'
#'
#' @import ggplot2


plotMCA_server <- function(id,
                           plot_DF,
                           eigenvalues,
                           active_entity,
                           active_dims) {
  moduleServer(id, function(input, output, session) {
    # UI RENDERING ------------------------------------------------------------

    output$render_order_by <- renderUI({
      req(!is.null(plot_DF()))
      ns <- session$ns
      tagList(
        pickerInput(
          ns("order_by"),
          "Color observations by importance of...",
          choices = active_dims(),
          selected = "dim_x"
        ),

        actionButton(ns("save"),
                     "Save")
      )
    })

    # RENDER ENTITIES PLOT
    output$MCA_plot <- renderPlot({
      req(!is.null(plot_DF()))
      validate(need(
        is.data.frame(plot_DF()),
        "There are no results to be plotted"
      ))
      validate(need(length(active_dims()) ==2,
                    "Two dimensions must be selected"))
      create_entities_ggplot(plot_DF(),
                    active_entity(),
                    active_dims(),
                    input$order_by)
    })

    observeEvent(input$save, {
      ggsave("inst/test-plot.pdf",
             width = 17,
             height = 12,
             units = "in")
      showNotification("Plot saved!", duration = 2, type = "message")
    })

    # RENDER EIGENVALUES PLOT
    output$eigen_plot <- renderPlot({

      req(!is.null(plot_DF()))
      validate(need(
        length(eigenvalues()) > 0,
        "There are no results to be plotted"
      ))
      create_eigen_ggplot(eigenvalues = eigenvalues())
    })


  })
}
