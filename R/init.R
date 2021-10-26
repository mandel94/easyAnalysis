#' This files initializes the application state


imported <- shiny::reactiveVal()
imported_DF <- reactiveVal()
selected <- shiny::reactiveVal()
include_bttn <- shiny::reactiveVal(0)
clear_bttn <- shiny::reactiveVal(0)
display_DF <- shiny::reactiveVal(data.frame())
displayed_vars <- shiny::reactiveVal(character(1))
MCA_output <- shiny::reactiveVal()
plot_DF <-  shiny::reactiveVal()
eigenvalues <- shiny::reactiveVal()
active_entity <- shiny::reactiveVal()
active_dims <- shiny::reactiveVal()
active_qualiquanti <- shiny::reactiveVal()
