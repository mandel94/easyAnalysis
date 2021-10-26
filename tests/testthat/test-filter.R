# # DEFINE TEST DATA AND VARIABLES  -----------------------------------------
#
# my_package <- "easyAnalysis"
#
# MCA_list_ind <-
#   readRDS(system.file("test-MCA_ind_out", package = my_package))
#
#
#
# # TESTING FILTERS ON INDIVIDUALS -------------------------------------------------------
#
#
# test_that("apply_filter by contribution works on individuals", {
#   out <- apply_filter(MCA_list_ind,
#                       "contrib",
#                       2156,
#                       c("dim_1", "dim_2"))
#   expect_output(str(out), "data.frame")
#   expect_equal(ncol(out), 7)
#   expect_output(str(names(out)[1]), "index")
#   # Index must refer to individuals, not variables.
#   # The following test should be passed
#   expect_true(!is.na(as.numeric(out$index[1])))
# })
#
# test_that("apply_filter by cos2 works on individuals", {
#   out <- apply_filter(MCA_list_ind,
#                       "cos2",
#                       0.3,
#                       c("dim_1", "dim_2"))
#   expect_output(str(out), "data.frame")
#   expect_equal(ncol(out), 7)
#   expect_output(str(names(out)[1]), "index")
#   # Index must refer to individuals, not variables.
#   # The following test should be passed
#   expect_true(!is.na(as.numeric(out$index[1])))
# })
#
#
#
#
#
# # TESTING FILTERS ON VARIABLES  -------------------------------------------
#
# MCA_list_var <-
#   readRDS(system.file("test-MCA_var_out", package = my_package))
#
# test_that("apply_filter by contribution works on variables", {
#   out <- apply_filter(MCA_list_var,
#                       "contrib",
#                       10,
#                       c("dim_1", "dim_2"))
#   expect_output(str(out), "data.frame")
#   expect_equal(ncol(out), 7)
#   expect_output(str(names(out)[1]), "index")
#   # Index must refer to variables, not individuals.
#   # The following expression should give an error.
#   expect_error(out$index + 1)
# })
#
#
#
# test_that("apply_filter by cos2 works on variables", {
#   out <- apply_filter(MCA_list_var,
#                       "cos2",
#                       0.3,
#                       c("dim_1", "dim_2"))
#   expect_output(str(out), "data.frame")
#   expect_equal(ncol(out), 7)
#   expect_output(str(names(out)[1]), "index")
#   # Index must refer to variables, not individuals.
#   # The following expression should give an error.
#   expect_error(out$index + 1)
# })
#
#
#
#
# # TESTING FILTERS UTILITIES -----------------------------------------------
#
# test_best_set_by_cos2 <- data.frame(readRDS(system.file("inst/test-best_set_by_cos2",
#                                                         package="easyAnalysis")))
# test_best_set_by_contrib <- data.frame(readRDS(system.file("inst/test-best_set_by_contrib",
#                                                            package="easyAnalysis")))
# test_cos2_DF <- data.frame(readRDS(system.file("inst/test_cos2_DF",
#                                                package="easyAnalysis")))
# test_contrib_DF <- data.frame(readRDS(system.file("inst/test_contrib_DF",
#                                                   package="easyAnalysis")))
#
# names_are <- c("dim_1", "dim_2", "dim_3", "dim_4", "dim_5")
#
# names(test_cos2_DF) <- names_are
# names(test_contrib_DF) <- names_are
#
#
#
#
#
# test_that("merging with contrib works as expected", {
#   out <- merge_with_contrib(test_best_set_by_cos2,
#                             test_contrib_DF,
#                             dims = c("dim_1", "dim_2"))
#   expect_output(str(out), "data.frame")
#   expect_equal(ncol(out), 7)
#   expect_equal(nrow(out), nrow(test_best_set_by_cos2))
#   expect_true(all(out$index == test_best_set_by_cos2$index))
#
# })
#
#
# test_that("merging with cos2 works as expected", {
#   out <- merge_with_cos2(test_best_set_by_contrib,
#                          test_cos2_DF,
#                          dims = c("dim_1", "dim_2"))
#   expect_output(str(out), "data.frame")
#   expect_equal(ncol(out), 7)
#   expect_equal(nrow(out), nrow(test_best_set_by_contrib))
#   expect_true(all(out$index == test_best_set_by_contrib$index))
# })
#
#
#
#
#
#
