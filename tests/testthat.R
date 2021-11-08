#' Module for launching tests
#'

library(testthat)
library(shiny)
# library(easyAnalysis)
suppressWarnings(devtools::load_all("./"))

# test_check("easyAnalysis")
suppressWarnings(
  devtools::test()
)


