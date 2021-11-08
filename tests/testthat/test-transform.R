
# Inputs  -----------------------------------------------------------------

DF <- read_inst("inst/test-imported_data")
display_DF <- read_inst("inst/test-display_DF")



test_that("logic_AND returns right logic", {
  DF <- test_get_yes_no_DF(100, 3)

  AND_funct <- function(DF) {
    test_row <- function(row) {
      if (all(row %in% "Yes")) {
        "Yes"
      } else {
        "No"
      }
    }
    sapply(1:nrow(DF), function(i)
      test_row(DF[i,]))
  }

  expect_equal(logic_AND(DF), AND_funct(DF))

})


test_that("logic_OR returns right logic", {
  DF <- test_get_yes_no_DF(100, 3)

  OR_funct <- function(DF) {
    test_row <- function(row) {
      if (any(row %in% "Yes")) {
        "Yes"
      } else {
        "No"
      }
    }
    sapply(1:nrow(DF), function(i)
      test_row(DF[i,]))
  }

  expect_equal(logic_OR(DF), OR_funct(DF))

})


test_that("minimum_variance_filter works as expected", {
  out <- minimal_variance_filter(display_DF, 0.2)
  variances <- out %>%
    purrr::map(variance_binary)
  variances <- variances[which(variances > 0)]
  expect_true(all(0.2 < variances))
})



test_that("minimum_variance_filter works for categoricals", {
  display_DF$categorical <- sapply(rep(1, nrow(display_DF)),
                            function(for_each_row)
                            sample(c("cat_1", "cat_2", "cat_3"), for_each_row))
  out <- minimal_variance_filter(display_DF, 0.2)
  # THE FILTER SHOULD NOT BE APPLIED TO CATEOGORICAL VARIABLES
  expect_true("categorical" %in% names(display_DF))
})


test_that("logic_NOT returns right logic", {
  bin_var <- test_get_yes_no_DF(100, 1)

  not_bin_var <- ifelse(bin_var == "Yes", "No", "Yes")

  expect_equal(not_bin_var, sapply(bin_var, logic_NOT))

})



test_that("which_binary works as expected", {
  test_DF <- display_DF[, 1:5]
  test_DF$categorical <- sapply(rep(1, nrow(test_DF)),
                                function(x)
                                  sample(c("cat_1", "cat_2", "cat_3"), x))
  out <- unname(which_binary(test_DF))
  expect_equal(out, c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))

  test_DF_with_NA <- display_DF[, 1:5]
  test_DF_with_NA$categorical <- sapply(rep(1, nrow(test_DF)),
                                        function(x)
                                          sample(c("cat_1", "cat_2", "cat_3", NA), x))
  out <- unname(which_binary(test_DF_with_NA))
  expect_equal(out, c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE))
})


test_array <- display_DF$`Profiles demographic personal_attributes gender male`

test_that("is_binary works as expected", {
  out <- is_binary(test_array)
  expect_true(out)

  binary <- c("Yes", "No")
  out <- is_binary(binary)
  expect_true(is_binary(binary))

  binary_NA <- c("Yes", "No", NA)
  out <- is_binary(binary_NA)
  expect_true(is_binary(binary_NA))

  binary_short <- c("Yes")
  out <- is_binary(binary_short)
  expect_true(is_binary(binary_short))

  binary_short_NA <- c("Yes", NA)
  out <- is_binary(binary_short_NA)
  expect_true(is_binary(binary_short_NA))

  categorical <- c("cat_1", "cat_2", "cat_3")
  out <- is_binary(categorical)
  expect_false(is_binary(categorical))

  categorical_NA <- c("cat_1", "cat_2", "cat_3", NA)
  out <- is_binary(categorical_NA)
  expect_false(is_binary(categorical_NA))

  NA_array <- c(NA, NA, NA)
  out <- suppressWarnings(is_binary(NA_array))
  expect_warning(is_binary(NA_array))
})


test_that("variance_binary returns expected results", {
  len <- 100
  p <- 0.3
  variance <- p*(1-p)
  test_binary <- c(rep(1, p*len), rep(0, (1-p)*len))
  out <- variance_binary(test_binary)
  expect_equal(out, variance)
})

