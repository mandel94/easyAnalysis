#
#
# MCA_output <- readRDS(system.file("test-MCA_output", package = "easyAnalysis"))
#
#
# test_that('eigenvalue function works as expected', {
#   out_1 <- get_eigens(MCA_output,
#                       cumulative_percent = TRUE)
#   out_2 <- get_eigens(MCA_output, FALSE)
#   expect_type(out_1, "double")
#   expect_true(has_eigen_proper_order(out_2))
# })
#
#
#
#
#
#
#
