#' @import shiny

suppressWarnings(devtools::load_all())

easyAnalysis <- function() {

  ui <- fluidPage(
    stylesheet_dep(),
    apply_custom_css(),
    addBusyMessage(),
    navbarPage(
      "easyAnalysis",
      tabPanel("Import",
               UI_import()),
      tabPanel("Transform",
               UI_transform()),
      navbarMenu("Factorial Analysis",
                 tabPanel("MCA",
                          UI_MCA())
      )
    )
  )


  server <- function(input, output, session) {

    # Setting options
    options(shiny.maxRequestSize = 30 * 1024 ^ 2)


    importData_server("import", imported, imported_DF)

    selectVars_server("select", imported,
                      selected,
                      include_bttn,
                      clear_bttn)


    transformSurvey_server("transform_survey",
                           imported_DF,
                           display_DF,
                           displayed_vars)

    displayImport_server(
      "display_import",
      imported,
      selected,
      include_bttn,
      clear_bttn,
      display_DF,
      displayed_vars
    )

    displayTransform_server("display_transform",
                            display_DF)

    controlMCA_server("MCA_control",
                      display_DF,
                      displayed_vars,
                      active_entity,
                      active_dims)

    plotMCA_server("MCA_output",
                   plot_DF,
                   eigenvalues,
                   active_entity,
                   active_dims)


  }

  shinyApp(ui, server)
}

easyAnalysis()


