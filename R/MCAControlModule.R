#' Module for MCA Analysis Control
#'
#'
#' @param id The module's id
#'
#' @return \code{shiny.tag.list} object containing \code{shiny} UI components
#'
#' @importFrom shinyWidgets pickerInput actionBttn sliderTextInput
#' @importFrom FactoMineR MCA
#' @importFrom purrr map_lgl
#'


controlMCA_UI <- function(id) {
  ns <- NS(id)


  # USER INPUTS -------------------------------------------------------------

  tagList(
    wellPanel(
      tags$p("Add supplementary variables",
      class = "well-title"),
      uiOutput(ns("quanti_vars_list")),
      uiOutput(ns("quali_vars_list")),
      actionBttn(ns("start"),
                 "Start MCA",
                 style = "pill",
                 color = "primary")
    ),
    wellPanel(
      tags$p("Control MCA plot",
             class = "well-title"),
      pickerInput(
        ns("active_plot"),
        "Which entity should be plotted?",
        choices = c("Variables", "Individuals"),
        selected = "Variables"
      ),
      pickerInput(
        ns("dims"),
        "Which two dimensions should be plotted?",
        choices = list(
          dim_1 = "dim_1",
          dim_2 = "dim_2",
          dim_3 = "dim_3",
          dim_4 = "dim_4",
          dim_5 = "dim_5"
        ),
        selected = c("dim_1", "dim_2"),
        multiple = TRUE,
        options = list("max-options" = 2)
      )
    ),
    wellPanel(
      tags$p("Apply filters",
             class = "well-title"),
      pickerInput(
        ns("filter"),
        "Filter points by...",
        choices = c("cos2", "contrib"),
        selected = "cos2"
      ),
      uiOutput(ns("cos2_slider")),
      uiOutput(ns("contrib_slider"))
    )
  )
}




#' Server-side processing
#'
#' @param id The module's id
#' @param display_DF Reactive value, containing the data frame currently displayed
#'  in the import section
#' @param displayed_vars Reactive value, containing the names of the variables
#'  currently displayed in the import section
#' @param active_entity Reactive value, returning the active entity (individual,
#'  variable)
#' @param active_dims Reactive value, returning currently selected dimensions


controlMCA_server <- function(id,
                              display_DF,
                              displayed_vars,
                              active_entity,
                              active_dims) {
  moduleServer(id, function(input, output, session) {

    # REACTIVES  ---------------------------------------------------------------

    active_DF <- reactive({
      if (input$start == 0)
        data.frame()
      else {
        if (input$active_plot == "Variables")
          MCA_var_output()$coord
        else
          MCA_ind_output()$coord
      }
    })

    n_quali_quanti <- reactive({

      if(!is.null(quali_idx()) || !is.null(quanti_idx())) {
        n <- length(c(quali_idx(), quanti_idx()))
        n <- n[!is.null(n)]
      } else
        0
    })

    # Set the difference between the number of active variables and the sum of
    # qualitative variables and quantitative variables
    diff <- reactive({
      ncol(display_DF()[, -1]) - n_quali_quanti()
    })


    # RENDER UI'S -------------------------------------------------------------

    output$quanti_vars_list <- renderUI({
      ns <- session$ns
      pickerInput(
        ns("quanti_vars"),
        "Quantitative supplementary variables",
        multiple = TRUE,
        choices = displayed_vars()[-1],
      )
    })

    output$quali_vars_list <- renderUI({
      ns <- session$ns
      pickerInput(
        ns("quali_vars"),
        "Qualitative supplementary variables",
        multiple = TRUE,
        choices = displayed_vars()[-1]
      )
    })

    output$cos2_slider <- renderUI({

      ns <- session$ns
      req(input$start)
      validate(need(
        is.data.frame(display_DF()),
        "You still must select a data frame to display"
      ))

      value <- reactive({

        if (is.null(isolate(input$cos2)))
          0
        else
          isolate(input$cos2)
      })

      if (input$filter == "cos2") {
        if (!is.data.frame(display_DF()))
          tags$p("No dataframe selected")
        else {
          if (nrow(isolate(active_DF())) > 0) {
            # Keep the top 'input$contrib' points (ordered by how much they contribute
            # to plotted dimensions)
            sliderInput(
              ns("cos2"),
              "cos2",
              min = 0,
              max = 1,
              value = value(),
              step = 0.1
            )
          } else {
            tags$p("")
          }
        }
      }
    })

    output$contrib_slider <- renderUI({

      ns <- session$ns
      req(input$start)
      validate(need(nrow(isolate(active_DF(

      ))) > 0, "Data frame is empty"))

      value <- reactive({

        if (is.null(isolate(input$contrib)))
          if (nrow(active_DF()) == 0)
            1
        else
          nrow(active_DF())
        else
          nrow(active_DF())
      })

      if (input$filter == "contrib") {
        if (!is.data.frame(display_DF()))
          tags$p("No dataframe selected")
        else {
          max_value <- reactiveVal(nrow(active_DF()))
          if (max_value() > 0) {
            # Keep the top 'input$contrib' points (ordered by how much they contribute
            # to plotted dimensions)
            sliderInput(
              ns("contrib"),
              "contrib",
              min = 1,
              max = max_value(),
              value = value(),
              step = 1
            )
          } else {
            tags$p("")
          }
        }
      }
    })


    # OBSERVERS ---------------------------------------------------------------


    observeEvent(input$active_plot, {

      active_entity(input$active_plot)
    })


    observeEvent(input$dims, {
      active_dims(input$dims)
    })


    observeEvent(filtered_DF(), {

      req(input$start)
      plot_DF(filtered_DF())
    })

    observeEvent(eigens(), {
      eigenvalues(eigens())
    })

    # BUILD MCA ---------------------------------------------------------------


    quanti_idx <- reactive({
      if (length(input$quanti_vars) > 0) {
        quanti_input_idx <- which(displayed_vars() %in% input$quanti_vars) - 1
        # Check which variable is numerical
        is_quanti <- display_DF()[,-1] %>%
          select(quanti_input_idx) %>%
          map_lgl(is.numeric)
        if (all(!is_quanti))
          NULL
        else
          quanti_input_idx[is_quanti]
      }
      else
        NULL
    })

    quali_idx <- reactive({

      if (length(input$quali_vars) > 0) {
        quali_input_idx <- which(displayed_vars() %in% input$quali_vars) - 1
        # Check which variable is numerical
        is_quanti <- display_DF()[, -1] %>%
          select(quali_input_idx) %>%
          map_lgl(is.numeric)
        if (all(is_quanti))
          NULL
        else
          quali_input_idx[!is_quanti]
      } else
        NULL
    })

    qualiquanti_names <- reactive({

      names(display_DF())[-1][c(quali_idx(), quanti_idx())]
    })

    # Reactive containing output of the MCA
    MCA_res <- reactive({
      req(is.data.frame(display_DF()))
      req(ncol(display_DF()) > 1)
      req(input$start)
      if(isolate(diff()) < 2) {
        showNotification(tags$p("You must select at least two active variables",
                                class = "warning"))
        validate(need(diff() >= 2, ""))
      }

      FactoMineR::MCA(
        display_DF()[, -1],
        quanti.sup = quanti_idx(),
        quali.sup = quali_idx(),
        graph = FALSE
      )
    })

    # The following reactives contain the list with:
    # - the biplot coordinates;
    # - the cos2 (how important each dimension is to the entity)
    # - the contribution (how much does the entity contribute to each
    #   dimension?)
    # ...
    # When entities are variables
    MCA_var_output <- reactive({
      req(names(display_DF()) != "ID_CODE")
      labeled_vals <- get_labeled_values(display_DF())
      # List, each element with 5 columns (and category as rownames)
      get_var_output(MCA = MCA_res(), labeled_vals)
    })

    # When entities are individuals
    MCA_ind_output <- reactive({
      id <- display_DF()[, 1][[1]]
      ind_output <- get_ind_output(MCA = MCA_res())

      biplot_coords <- data.frame(ind_output$coord)
      rownames(biplot_coords) <- id
      biplot_coords <- select(biplot_coords, c(everything()))

      contributions <- data.frame(ind_output$contrib)
      rownames(contributions) <- id

      cos2 <- data.frame(ind_output$cos2)
      rownames(cos2) <- id

      ind_output$coord <- biplot_coords
      ind_output$contrib <- contributions
      ind_output$cos2 <- cos2

      # List, each element with 5 columns (and ID_CODE as rownames)
      ind_output
    })

    eigens <- reactive({
      req(input$start)
      get_eigens(MCA_res(), cumulative_percent = FALSE)
    })


    # APPLY FILTERS -----------------------------------------------------------

    filter_value <- reactive({
      if (input$filter == "cos2") {
        if (is.null(input$cos2))
          0
        else
          input$cos2
      }
      else {
        if (is.null(input$contrib))
          nrow(isolate(active_DF()))
        else if (input$contrib > nrow(active_DF()))
          nrow(isolate(active_DF()))
        else
          input$contrib
      }
    })

    filtered_DF <- reactive({

      req(input$start)
      validate(need(length(input$dims) == 2, "Two dims must be selected"))
      if (input$active_plot == "Variables") {
        out <- apply_filter(MCA_var_output(),
                     input$filter,
                     filter_value(),
                     input$dims)
        qualiquanti_out <- get_qualiquanti_out(MCA_var_output(), input$dims)
        out_ext <- merge_qualiquanti(out, qualiquanti_out)
        out_ext
      } else {
        apply_filter(MCA_ind_output(),
                     input$filter,
                     filter_value(),
                     input$dims)
      }
    })

  })
}
