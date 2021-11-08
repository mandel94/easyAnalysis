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


MCA_var <- read_inst("inst/test-var_MCA_output")$coord
display_DF <- read_inst("inst/test-display_DF_for_MCA")
labeled_vals <- get_labeled_values(display_DF)


test_that("get_labeled_values works as expected", {
  expect_type(labeled_vals, "character")
  expect_true(length(labeled_vals) != 0)
  expect_true(sum(is.na(labeled_vals)) == 0)
})

# CONTINUE
test_that("map_rownames_to_labeled_values works as expected", {
  out <- map_rownames_to_labeled_values(MCA_var, labeled_vals)

  # Each row of MCA_var should map to a unique labeled value
  expect_true(length(unique(out)) == nrow(MCA_var))
})



