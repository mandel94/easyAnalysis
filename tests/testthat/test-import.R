
DF <- read_inst("inst/ test-imported_data")
test_that("descriptive data transformation preserves original data", {
  out <- get_descriptive_binaries(DF)
  expect_true(is.data.frame(out))
  expect_true(does_info_match(DF, out))
})
