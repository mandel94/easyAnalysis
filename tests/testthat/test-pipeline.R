#' #' This modules tests integrated logical units of functionalities
#' #'
#' #'
#' #' DEFINE PIPELINE:
#' #' \begin{enumerate}
#' #'  \item Import the data
#' #'  \item Display the data
#' #'  \item (Transform the data)
#' #'  \item Apply MCA
#' #'  \item Get dataframe to display
#' #' \end{enumerate}
#' #'
#'
#'
#'
#' # TEST OUTPUTS (INITIALIZE)
#' output_test_1 <- NA
#' output_test_2 <- NA
#' output_test_3 <- NA
#' output_test_4 <- list("Variables" = NULL,
#'                       "Individuals" = NULL)
#'
#'
#'
#'
#'
#' # TEST 1: IMPORT  -----------------------------------------------------------------
#'
#' input_csv <- system.file("inst/0201_01_02_attitudini_dati.csv",
#'                          package = "easyAnalysis")
#' input_xml <- system.file("inst/0201_01_02_attitudini.xml",
#'                          package = "easyAnalysis")
#'
#' testServer(importData_server, args = list(imported = reactiveVal(NULL)), {
#'   session$setInputs(
#'     csv_file = list(datapath = input_csv),
#'     xml_file = list(datapath = input_xml),
#'     import = 0
#'   )
#'
#'   # EXPECTATIONS
#'   expect_output(str(session$returned()[[1]]), "data.frame")
#'   expect_true(ncol(session$returned()[[1]]) > 0)
#'   expect_true(nrow(session$returned()[[1]]) > 0)
#'
#'   output_test_1 <<- imported()
#' })
#'
#'
#'
#'
#' # TEST 2: DISPLAY -----------------------------------------------------------------
#'
#'
#' # testServer(
#' #   displayImport_server,
#' #   args = list(
#' #     imported = output_test_1,
#' #     selected = reactiveVal(NULL),
#' #     include_bttn = reactiveVal(NULL),
#' #     clear_bttn = reactiveVal(NULL),
#' #     display_DF = reactiveVal(NULL),
#' #     displayed_vars = reactiveVal(NULL)
#' #   ),
#' #   {
#' #
#' #   }
#' # )
#'
#'
#'
#'
#' # TEST 4: MCA -------------------------------------------------------------
#'
#' active_plots <- c("Variables", "Individuals")
#' filters <- c("cos2", "contrib")
#'
#' # TEST 'NULL' VALUES FOR FILTERS
#' testServer(controlMCA_server,
#'            args = list(
#'              display_DF = reactive(output_test_1[[1]]),
#'              displayed_vars = names(output_test_1[[1]])
#'            ),
#'            {
#'              contribs <- c(368, 2156)
#'              i <- 1
#'              for (a_p in active_plots) {
#'                for (f in filters) {
#'                  setInputs_test_plot_DF(a_p,
#'                                         f,
#'                                         session,
#'                                         cos2_value = 0,
#'                                         contrib_value = contribs[i])
#'                  expect_output(str(plot_DF()), "data.frame")
#'                  expect_true(ncol(plot_DF()) > 0)
#'                  # EXPECT NO OBSERVATION TO BE FILTERED OUT
#'                  expect_true(nrow(plot_DF()) == nrow(active_DF()))
#'                }
#'                i <- i + 1
#'              }
#'            })
#'
#'
#' # TEST INTERMEDIATE VALUES FOR FILTERS
#' testServer(controlMCA_server,
#'            args = list(
#'              display_DF = reactive(output_test_1[[1]]),
#'              displayed_vars = names(output_test_1[[1]])
#'            ),
#'            {
#'              for (a_p in active_plots) {
#'                for (f in filters) {
#'                  setInputs_test_plot_DF(a_p,
#'                                         f,
#'                                         session,
#'                                         cos2_value = 0.2,
#'                                         contrib_value = 100)
#'                  expect_output(str(plot_DF()), "data.frame")
#'                  expect_true(ncol(plot_DF()) > 0)
#'                }
#'                output_test_4[[a_p]] <<- plot_DF()
#'              }
#'
#'            })
#'
#'
#' # TEST 'EXTREME' VALUES FOR FILTERS
#' testServer(controlMCA_server,
#'            args = list(
#'              display_DF = reactive(output_test_1[[1]]),
#'              displayed_vars = names(output_test_1[[1]])
#'            ),
#'            {
#'              for (a_p in active_plots) {
#'                for (f in filters) {
#'                  setInputs_test_plot_DF(a_p,
#'                                         f,
#'                                         session,
#'                                         cos2_value = 1,
#'                                         contrib_value = 1)
#'                  expect_output(str(plot_DF()), "data.frame")
#'                  expect_true(ncol(plot_DF()) > 0)
#'                }
#'              }
#'            })
#'
#'
#'
#'
#'
#'
#' # TEST 5: PLOT ------------------------------------------------------------
#'
#' testServer(plotMCA_server, args = list(plot_DF = output_test_4))
