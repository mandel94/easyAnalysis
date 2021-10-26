


test_take_input_files <- function(...) {
  files_list <- list(...)
  return(files_list)
}


test_get_rand_answers <- function(how_many) {
  levels <- c("Yes", "No")
  rand_answer <- function()
    sample(levels, 1)
  sapply(sequence(how_many), function(repeat_)
    rand_answer())
}

test_get_n_rows <- function(how_many_rows,
                            how_many_anwers) {
  rows <- vector("list", how_many_rows)
  for (row_i in 1:how_many_rows) {
    rows[[row_i]] <- test_get_rand_answers(how_many_anwers)
  }
  return(rows)
}

test_get_yes_no_DF <- function(rows, columns) {
  data.frame(matrix(unlist(
    test_get_n_rows(rows, how_many_anwers = columns)
  ),
  nrow = rows, ncol = columns))
}


has_eigen_proper_order <- function(eigens) {
  for (i in seq_along(eigens[-1])) {
    is_true <- eigens[i] >= eigens[i + 1]
    if (!is_true)
      return(FALSE)
  }
  TRUE
}

#' Specific-purpose Utility for Setting Test Inputs
#'
#' @param a_p String. The currently active plot
#' @param f String. Filter to be applied
#' @param cos2_value Numerical. Value assigned to cos2 filter
#' @param contrib_value Numerical. Value assigned to contrib filter
#' @param session. Shiny session object

setInputs_test_plot_DF <- function(a_p, f, cos2_value, contrib_value, session) {
  if (f == "cos2") {
    session$setInputs(
      active_plot = a_p,
      start = 1,
      filter = f,
      cos2 = cos2_value,
      quanti_vars = NULL,
      quali_vars = NULL,
      dims = c("1", "2")
    )
  } else if (f == "contrib") {
    session$setInputs(
      active_plot = a_p,
      start = 1,
      filter = f,
      contrib = contrib_value,
      quanti_vars = NULL,
      quali_vars = NULL,
      dims = c("1", "2")
    )
  }
}

#' Read a RDS file from the inst/ directory
#' @param file_name Name of the file to be read

read_inst <- function(file_name) {
  readRDS(system.file(file_name, package = "easyAnalysis"))
}

#' Save a RDS file into the inst/ directory
#' @param file_name Name of the file to be read

save_inst <- function(obj, file_name) {
  saveRDS(obj, paste("inst/", file_name))
}

does_info_match <- function(DF1, DF2) {
  check_array <- rep(NA, ncol(DF1))
  i <- 1
  for (col in names(DF1)){
    check_array[i] <- if_one_then_yes(DF1[[col]], DF2[[col]])
    i <- i + 1
  }
  check_array <- check_array[!is.na(check_array)]
  if(all(check_array))
    TRUE
  else
    FALSE
}

if_one_then_yes <- function(array1, array2) {
  is_match <- rep(NA, length(array1))
  for (i in seq_along(array1)) {
    if (array1[i] == 1) {
      if (array2[i] == "Yes")
        is_match[i] <- TRUE
      else
        is_match[i] <- FALSE
    } else if (array1[i] == 0) {
      if (array2[i] == "No")
        is_match[i] <- TRUE
      else
        is_match[i] <- FALSE
    }
  }
  return(all(is_match))
}
