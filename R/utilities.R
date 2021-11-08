#' @import dplyr
#' @importFrom stringr str_extract str_detect str_c
#' @importFrom purrr map map_df map_lgl





# UI UTILITIES -----------------------------------------------


# well_title <- function() {
#   tagList(tags$p("Add supplementary variables",
#                  class = "well-title"), well_title_css())
# }




# IMPORT UTILITIES --------------------------------------------------------
test <-
  data.table::fread("../../testing/export-10000 RANDOM USERS from ZT and WR-18885647 _ 21-10-20_0955.csv")
#' get_descriptive_binaries
#'
#' Convert all 1/0 columns of the input data frame to Yes/No columns
#'
#' @param DF A data frame
#'

get_descriptive_binaries <- function(DF) {
  i <- 1
  DF %>%
    map_df(to_yes_no)
}


to_yes_no <- function(array_) {
  is_descriptive <-
    regexpr("(^yes$)|(^no$)", array_, ignore.case = TRUE)
  if (all(is_descriptive == 1, na.rm = TRUE)) {
    array_
  } else {
    sapply(array_, set_value)
  }
}

set_value <- function(v) {
  if (v == 1)
    "Yes"
  else if (v == 0)
    "No"
  else
    v
}

#' remove_rows_with_NAs
#'
#' This function takes a data frame as input, and removes all rows having at
#'  at least one missing value.
#'
#' @param DF A data frame
#' @return A data frame

remove_rows_with_NAs <- function(DF) {
  to_keep <-
    DF %>%
    map_df(function(x) is.na(x)) %>%
    rowSums(na.rm = TRUE) %>%
    map_lgl(function(s) s == 0)
  DF[to_keep, ]
}


# TRANSFORMATION UTILITIES ------------------------------------------------

# Return input dataframe with all its columns being numerical
all_to_numericals <- function(DF) {
  if (sum(sapply(DF, is.numeric)) < ncol(DF)) {
    return(sapply(DF, to_numeric))
  }
  return(DF)
}

# Covert survey responses to binaries (1 if affirmative, 0 if negative)
to_numeric <- function(var) {
  if(is.numeric(var))
    var
  else {
    apply_regex <- function(x) regexpr("yes", x, ignore.case = TRUE) == 1
    ifelse(sapply(var, apply_regex) == 1, 1, 0)
  }
}

# AND operation
logic_AND <- function(DF) {
  sums <- DF %>%
    all_to_numericals() %>%
    rowSums()
  to_return <- ifelse(sums == ncol(DF), "Yes", "No")
  unname(to_return)
}

# OR operation
logic_OR <- function(DF) {
  sums <- DF %>%
    all_to_numericals() %>%
    rowSums()
  to_return <- ifelse(sums > 0, "Yes", "No")
  unname(to_return)
}

# NOT operation
logic_NOT <- function(var) {
  var <- to_numeric(var)
  to_return <- ifelse(var == 1, "No", "Yes")
  unname(to_return)
}


#' Apply Logical Transformation to Selected Variables
#'
#' This function is designed for applying logical operation to the set of sele-
#' cted variables, extracted from a given dataframe.
#'
#' @param DF A data frame
#' @param selected Character array containign which variables are to be included
#'  in the logical operation
#' @param operation String, specifying which logical operation is to be applied
#'  to the set of selected variables. Admitted values are:
#'  \describe {
#'    \item{AND}
#'    \item{OR}
#'    \itemè{NOT --> Only works for one variables set}
#'    }
#' @param new_var_name String, naming the newly created variable
#' @param discard_selected Logical. Should variables from the transformation pool
#'  be discarded?
#'
#' @return dataframe, containing the results of the logical operation


transform_ <- function(DF,
                       selected,
                       operation,
                       new_var_name,
                       discard_selected) {
  DF_numerical <- DF %>%
    select(all_of(selected)) %>%
    all_to_numericals()

  funct_to_apply <- paste0("logic_", operation, "(DF_numerical)")

  funct_out <- eval(parse(text = funct_to_apply))

  to_return <- data.frame(funct_out)

  if (!discard_selected) {
    for (var in selected) {
      to_return <- cbind(to_return, DF[, var])
    }
    names(to_return) <-  c(new_var_name, selected)
  } else {
    names(to_return) <-  new_var_name
  }

  to_return
}





#' Get input DF mutated with input variables
#'
#'
#' @param DF A data frame
#' @param transform_out Data frame containing the result of the previous
#'  transformation, to be added to \code{DF}
#' @param selected Character array containing which variables were included
#'  in the logical operation
#'
#' @return a data frame, mutated with arrays resulting from previous transforma-
#'  tion step

get_mutated_DF <- function(DF, transform_out, selected) {
  if (ncol(transform_out) == 1) {
    DF <- select(DF,-all_of(selected)) %>%
      mutate(transform_out)
  } else {
    DF <- mutate(DF, transform_out)
  }
  return(DF)
}


#' Apply Variance Filter to Variables
#'
#' This function takes a data frame, and transforms it by keeping only those
#'  binary variables whose variance is greater than a given threshold.
#'  Non-binary variables are kept as they are
#'
#' @param DF A data frame
#' @param threshold num. Variables whose variance is lesser than
#'  \code{threshold} is removed from \code{DF}
#'
#' @return \code{DF}, with variables within the given variance threshold removed

minimal_variance_filter <- function(DF, threshold) {

  is_binary <- which_binary(DF)

  if (all(!is_binary))
    return(DF)

  keep_idx <- rep(TRUE, ncol(DF))
  variance_array <- rep(NA, ncol(DF))
  i <- 1
  for (x in is_binary) {
    if (x == TRUE) {
      variance <- variance_binary(DF[, i])
      variance_array[i] <- variance
    }
    i <- i + 1
  }
  i <- 1
  for (x in variance_array) {
    if (!is.na(x)) {
      if (variance_array[i] < threshold)
        keep_idx[i] <- FALSE
    }
    i <- i + 1
  }
  DF[, keep_idx, drop=FALSE]
}


#' Compute Variance of Binary Variable
#'
#' This function returns the variance of a binary variable, compute as
#'  \code{p(1-p)}, where \code{p} is the proportion of ones in the data (this
#'  is the variance of a bernoullian distribution)
#'
#' @param bin_array Array of binary data (1/0, or TRUE/FALSE)

variance_binary <- function(bin_array) {
  bin_array <- to_numeric(bin_array)
  p <- sum(bin_array, na.rm = TRUE) / length(bin_array)
  n_of_NAs <- sum(is.na(bin_array))
  variance <- p * (1 - p)
  NA_alert_threshold <- 0.3 * length(bin_array)
  if (n_of_NAs >= NA_alert_threshold) {
    warning(
      "There is a considerable amount of NA's in the data! Be aware that
            `variance_binary` doesn't consider missing value when computing
            variance"
    )
    return(variance)
  }
  return(variance)
}


#' Utility for detecting binary columns of a data frame
#'
#' @param DF A data frame
#'
#' @return Logical array of TRUE and FALSE
#'
#' @importFrom purrr map_lgl

which_binary <- function(DF) {
  DF <- DF %>%
    map_lgl(is_binary)
}


#' Utility returning TRUE for binary data, FALSE for non-binary data
#'
#' @param Array
#'
#' @return Logical value

is_binary <- function(array_) {
  na_detected <- sum(is.na(array_)) > 0
  levls <- levels(factor(array_))
  are_yes_no <- regexpr("(^yes$)|(^no$)", levls, ignore.case = TRUE)
  if (length(levls) == 2) {
    if (na_detected) {
      if (all(are_yes_no))
        return(TRUE)
      else
        return(FALSE)
    } else
      return(TRUE)
  } else if (length(levls) == 0) {
    if (na_detected) {
      warning("Input is an array of NA's")
      return(FALSE)
    } else
      return(FALSE)
  } else if (length(levls) == 1) {
    if (all(are_yes_no))
      return(TRUE)
    else
      return(FALSE)
  } else if (length(levls) > 2) {
    return(FALSE)
  }
}


# MCA UTILITIES -----------------------------------------------------------

#' Utility for getting MCA output for active variables
#'
#' @param MCA MCA object
#' @param labeled_values Array, with values labeled by category
#'


get_var_output <- function(MCA, labeled_values) {
  return_list <- list()

  MCA[["var"]]

  for (out in c("coord", "contrib", "cos2")) {
    return_list[[out]] <- add_labeled_values(MCA[["var"]][[out]], labeled_values)
  }
  for (out in c("quali.sup", "quanti.sup")) {
    out_DF <- add_labeled_values(MCA[[out]]$coord, labeled_values)
    if(!is.null(out_DF))
     return_list[[out]] <- out_DF
  }
  return_list
}


# Utility for getting MCA output for active individuals
get_ind_output <- function(MCA) {
  return_list <- list()

  for (out in c("coord", "contrib", "cos2")) {
    return_list[[out]] <- MCA[["ind"]][[out]]
  }

  return_list
}

#' Utility function for getting eigenvalues of MCA factors (MCA is performed
#'  using FactoMineR package)
#'
#' @param MCA MCA object
#' @param cumulative_percent Logical. It True, extract the cumulative percentages
#'  of explained variance, starting from the first MCA factor
#'
#' @return numerical array, with length equal to the number of MCA factor,
#'  containing the eigenvalues associated with each of those factors, or the cumu-
#'  lative percentages of explained variance if \code{cumulative_percent} is TRUE

get_eigens <- function(MCA,
                       cumulative_percent) {
  if (cumulative_percent)
    MCA$eig[, 3]
  else
    MCA$eig[, 1]
}


get_qualiquanti_out <- function(MCA_res, dims) {
  return_list <- list()
  dims <- sapply(dims, function(x)
    unlist(strsplit(x, "_"))[2])
  if (!is.null(MCA_res$quali.sup)) {
    quali_DF <- data.frame(MCA_res$quali.sup)[, as.integer(dims)]
    quali_DF$index <- row.names(quali_DF)
    quali_DF$cos2_x <- rep(NA, nrow(quali_DF))
    quali_DF$cos2_y <- rep(NA, nrow(quali_DF))
    quali_DF$contrib_x <- rep(NA, nrow(quali_DF))
    quali_DF$contrib_y <- rep(NA, nrow(quali_DF))
    quali_DF$qualiquanti <- rep(1, nrow(quali_DF))
    quali_DF <- quali_DF %>%
      select(index, everything())
    names(quali_DF) <- c(
      "index",
      "dim_x",
      "dim_y",
      "cos2_x",
      "cos2_y",
      "contrib_x",
      "contrib_y",
      "qualiquanti"
    )
    return_list$quali_DF <- quali_DF
  } else
    return_list$quali_DF <- NULL

  if (!is.null(MCA_res$quanti.sup)) {
    quanti_DF <- data.frame(MCA_res$quanti.sup[, as.integer(dims)])
    quanti_DF$index <- row.names(quanti_DF)
    quanti_DF$cos2_x <- rep(NA, nrow(quanti_DF))
    quanti_DF$cos2_y <- rep(NA, nrow(quanti_DF))
    quanti_DF$contrib_x <- rep(NA, nrow(quanti_DF))
    quanti_DF$contrib_y <- rep(NA, nrow(quanti_DF))
    quanti_DF$qualiquanti <- rep(1, nrow(quanti_DF))
    quanti_DF <- quanti_DF %>%
      select(index, everything())
    names(quanti_DF) <- c(
      "index",
      "dim_x",
      "dim_y",
      "cos2_x",
      "cos2_y",
      "contrib_x",
      "contrib_y",
      "qualiquanti"
    )

    return_list$quanti_DF <- quanti_DF
  } else
    return_list$quanti_DF <- NULL
  return_list
}


#' Extend output of MCA
#' @param out Dataframe, with output of entities
#' @param qualiquanti_out list, with output of qualitative and quantitative variables
merge_qualiquanti <- function(out, qualiquanti_out) {
  out$qualiquanti <- rep(0, nrow(out))
  out_ext <-
    rbind(out, qualiquanti_out$quali_DF, qualiquanti_out$quanti_DF)
  out_ext
}


#' Add Labeled Rownames
#'
#' When applying MCA, original category names are pre-appended to the
#'  answer only for binary variables. This function is used for changing the row
#'  names of the input DF, pre-appending the category names for all types of
#'  variable.
#'
#'
#' @param DF A data frame
#' @param labeled_values Array, with values labeled by category
#'
#' @return A data frame, the same as the input one, but with rownames modified
#'  with the labeled version of variable values.

add_labeled_values <- function(DF, labeled_values) {
  # FIND A MATCH BETWEEN ROWNAMES AND LABELED_VALUES, THAT IS,
  # FOR EACH ROWNAME, THE INDEX OF THE MATCHING LABELED VALUE
  mapping <- map_rownames_to_labeled_values(DF, labeled_values)
  # MAP THE ROWNAME TO THE LABELED_VALUE
  for (i in seq_along(row.names(DF))) {
    rownames(DF)[i] <- labeled_values[mapping[i]]
  }

  DF
}

get_labeled_values <- function(DF) {
  long_table <- tidyr::pivot_longer(DF, !all_of("ID_CODE"), "categories")
  long_table$value[is.na(long_table$value)] <- "NA"
  labeled_values <- str_c(long_table$categories, long_table$value, sep = "_")
  return(unique(labeled_values))
}

# Get the mapping between the rownames and the labeled value
map_rownames_to_labeled_values <- function(DF, labeled_values) {
  foo <- sapply(row.names(DF), function(x) get_match_index(x, labeled_values))
  return(unlist(foo))
}

#' Find which labeled_value match to a certain row name.
get_match_index <- function(row_name, labeled_values) {
  row_name <- str_replace(row_name, "\\.", "_")
  split <- unlist(strsplit(row_name, "_"))
  is_already_labeled <- length(split) >= 2
  if (is_already_labeled) {
    # ´grep(value = FALSE) returns a vector with the indices of the elements of
    #  x that yielded a match´
    which(labeled_values %in% row_name)
  } else {
    grep(paste0("_", row_name, "$"), labeled_values)
  }
}

# VALIDATION UTILITIES  ---------------------------------------------------

validate_update <- function(selected, operator) {
  if (operator %in% c("AND", "OR")) {
    if (length(selected) >= 2)
      TRUE
    else
      FALSE
  } else {
    if (length(selected) == 1)
      TRUE
    else
      FALSE
  }
}




# FILTER UTILITIES  --------------------------------------------------------

#' Get Filter Value
#'
#' this function returns the current value of the filter (be it cos2 or contrib)
#'
#' @param filter Name of the filter to be applied
#' @param cos2 User-set value of cos2 filter
#' @param contrib User-set value of contrib filter
#' @param active_DF Currently active data frame
#'
get_filter_value <- function(filter, cos2, contrib, active_DF) {
  if (filter == "cos2") {
    if (is.null(cos2))
      0
    else
      cos2
  }
  else {
    if (is.null(contrib))
      nrow(isolate(active_DF))
    else
      contrib
  }
}


#' Apply Filter to MCA object
#'
#' @param MCA_list List containing output from MCA.
#' @param filter Filter type. It can be "variables" or "individuals", depending on
#'  which plot is currently active
#' @param value num. Value of the filter to be applied
#' @param dims array informing about currently active dimensions
#'
#'  If \code{filter = 'cos2'}, only rows with cos2 greater than or equal to
#'   \code{value} in at least one of plotted dimensions will be kept.
#'  If \code{filter = 'contrib'}, only the top \code{value} observations with
#'   best contribution in at least one of the dimensions being displayed. For example,
#'   if \code{value = 5} and \code{filter = 'contrib'}, an observation won't be
#'   displayed if it does not rank among the top 5 contributions in any of
#'   displayed dimensions.
#'
#' @return  Data frame with only those observations from \code{DF} that pass the
#'  filtering criteria, and the columns corresponding to current active dimensions

apply_filter <- function(MCA_list,
                         filter,
                         value,
                         dims) {
  dims <- dims %>%
    sapply(function(x)
      strsplit(x, "_")[[1]][2])
  dims <- as.numeric(dims)
  DF <- data.frame(MCA_list$coord[, c(dims)])

  if (filter == "contrib") {
    contribs <- data.frame(MCA_list$contrib[, c(dims)])

    best_set <-
      get_best_set_by_contrib(DF, contribs, value)

    best_set <- merge_with_cos2(best_set, MCA_list$cos2, dims)

    best_set
  }
  else if (filter == "cos2") {
    cos2 <- data.frame(MCA_list$cos2[, c(dims)])
    best_set <-
      get_filtered_by_cos2(DF, cos2, value)

    best_set <- merge_with_contrib(best_set, MCA_list$contrib, dims)
    best_set
  }
}



#' Get Best Set by Contribution
#'
#' This function is used for filtering observation in a data frame according to
#'  their contribution to dimensions
#'
#' @param DF Data frame
#' @param contribs Data frame containing contributions to active dimensions
#' @param value int. The value of the contribution filter
#'
#' @return Data frame with only those observations from \code{DF} that pass the
#'  filtering criteria

get_best_set_by_contrib <- function(DF, contribs, value) {
  dims_match <- stringr::str_extract(names(DF), "\\.(.)")
  dim_x <- str_replace(dims_match[1], "\\.", "")
  dim_y <- str_replace(dims_match[2], "\\.", "")
  merged_DF <- merge(DF, contribs, by = 'row.names')
  # Let's assign generic subscript the the active dimensions ('x' and 'y')
  names(merged_DF) <-
    c("index", "dim_x", "dim_y", "contrib_x", "contrib_y")
  # Initialize vector for inclusion in 'best-set'
  merged_DF[["top_dim_x"]] <- rep(FALSE, nrow(merged_DF))
  merged_DF[["top_dim_y"]] <- rep(FALSE, nrow(merged_DF))

  # Arrange merged_DF by contrib_x
  merged_DF <- arrange(merged_DF, desc(contrib_x))
  # Switch top x observations to TRUE

  merged_DF[["top_dim_x"]][1:value] <- TRUE

  #Arrange merged_DF by contrib_y
  merged_DF <- arrange(merged_DF, desc(contrib_y))
  # Switch top y observations to TRUE
  merged_DF[["top_dim_y"]][1:value] <- TRUE

  # Extract index of observations to keep, i.e. observation that are included
  # either in the top_x set or in the top_y set

  filter <-
    merged_DF[, "top_dim_x"] | merged_DF[, "top_dim_y"]
  filtered_DF <-
    merged_DF[filter, c("index", "dim_x", "dim_y", "contrib_x", "contrib_y")]
  names(filtered_DF) <- c(
    "index",
    paste0("dim_", dim_x),
    paste0("dim_", dim_y),
    paste0("contrib_", dim_x),
    paste0("contrib_", dim_y)
  )
  filtered_DF

}

#' Get Data Frame filtered By Cos2
get_filtered_by_cos2 <- function(DF, cos2, value) {
  dims_match <- stringr::str_extract(names(DF), "\\.(.)")
  dim_x <- str_replace(dims_match[1], "\\.", "")
  dim_y <- str_replace(dims_match[2], "\\.", "")
  merged_DF <- merge(DF, cos2, by = 'row.names')
  names(merged_DF) <-
    c("index", "dim_x", "dim_y", "cos2_x", "cos2_y")

  filter <- merged_DF %>%
    cos2_filter(value)
  filtered_DF <-
    merged_DF[filter, c("index", "dim_x", "dim_y", "cos2_x", "cos2_y")]
  names(filtered_DF) <- c(
    "index",
    paste0("dim_", dim_x),
    paste0("dim_", dim_y),
    paste0("cos2_", dim_x),
    paste0("cos2_", dim_y)
  )
  filtered_DF
}

#' Return TRUE if cos2 is greater than or equal to the filter value,
#' for at least one of the two dimensions, FALSE otherwise.
cos2_filter <- function(DF, value) {
  x <- DF$cos2_x
  y <- DF$cos2_y
  is_greater_x <- ifelse(x >= value, TRUE, FALSE)
  is_greater_y <- ifelse(y >= value, TRUE, FALSE)
  return(is_greater_x | is_greater_y)
}


merge_with_cos2 <- function(best_set, cos2_DF, dims) {
  cos2_DF <- data.frame(cos2_DF)[, dims] %>%
    mutate(index = rownames(cos2_DF))
  if (nrow(best_set) == 0) {
    best_set$cos2_x <- character(0)
    best_set$cos2_y <- character(0)
    names(best_set) <- names(best_set) %>%
      sapply(replace_with_x, dims) %>%
      sapply(replace_with_y, dims)
    return(best_set)
  }

  best_set$original_row_order <- seq(1:nrow(best_set))
  merged_DF <- merge(best_set, cos2_DF, by = "index") %>%
    arrange(original_row_order)
  merged_DF$original_row_order <- NULL
  names(merged_DF)[6:7] <- c("cos2_x", "cos2_y")
  names(merged_DF) <- names(merged_DF) %>%
    sapply(replace_with_x, dims) %>%
    sapply(replace_with_y, dims)
  return(merged_DF)

}

merge_with_contrib <- function(best_set, contrib_DF, dims) {
  contrib_DF <- data.frame(contrib_DF)[, dims] %>%
    mutate(index = rownames(contrib_DF))
  if (nrow(best_set) == 0) {
    best_set$contrib_x <- character(0)
    best_set$contrib_y <- character(0)
    names(best_set) <- names(best_set) %>%
      sapply(replace_with_x, dims) %>%
      sapply(replace_with_y, dims)
    return(best_set)
  }
  best_set$original_row_order <- seq(1:nrow(best_set))
  merged_DF <- merge(best_set, contrib_DF, by = "index") %>%
    arrange(original_row_order)
  merged_DF$original_row_order <- NULL
  names(merged_DF)[6:7] <- c("contrib_x", "contrib_y")
  names(merged_DF) <- names(merged_DF) %>%
    sapply(replace_with_x, dims) %>%
    sapply(replace_with_y, dims)
  return(merged_DF)
}


replace_with_x <- function(name, dims) {
  dim <- dims[1]
  str_replace_all(name, paste0("_", dim), "_x")
}

replace_with_y <- function(name, dims) {
  dim <- dims[2]
  str_replace_all(name, paste0("_", dim), "_y")
}

detect_qualiquanti <- function(string, char_array) {
  detect_res <- rep(NA, length(char_array))
  i <- 1
  for (c in char_array)
    detect_res[[i]] <- str_detect(string, c)
  any(detect_res)
}


# PLOT UTILITIES  ---------------------------------------------------------

#' Create Ggplot Object for Entities (Variables or Individuals)
#'
#' @param DF Data frame. The data to pass to ggplot
#' @param active_dims Array of length 2. Currently active dimensions
#' @param active_entity String. Currently active entity (variables vs
#'  individuals)
#' @param active_qualiquanti Reactive value, returning the array of currently
#'  active qualitative and quantitative variables. Active variables are obtained
#'  by taking the input given by the user and filtering out the variables whose
#'  data types is not coherent with the category of the input (quantitative or
#'  qualitative).
#' @param order_by String. Must to be one of currently active dimensions.
#'  Map the color of plotted points by cos2 of selected dimension
#'
#'
#'

create_entities_ggplot <-
  function(DF, active_entity, active_dims, order_by) {
    if (is.null(order_by))
      return(NULL)
    filters <- c("cos2_x", "cos2_y")
    order_by_idx <- which(active_dims == order_by)
    order_by <- filters[order_by_idx]
    active_color <- rgb(0 / 355, 0, 0 / 355, 0.6)
    qualiquanti_color <- rgb(255 / 355, 164 / 355, 0, 0.8)
    if (!is.null(DF$qualiquanti)) {
      DF_active <- DF[DF$qualiquanti == 0, ]
      DF_qualiquanti <- DF[DF$qualiquanti == 1, ]
    } else {
      DF_active <- DF
      DF_qualiquanti <-
        data.frame(dim_x = character(0), dim_y = character(0))
    }
    base <- ggplot(DF) +
      geom_point(data = DF_active,
                 aes(dim_x,
                     dim_y,
                     colour = .data[[order_by]]),
                 size = 3) +
      geom_point(data = DF_qualiquanti,
                 aes(dim_x, dim_y),
                 color = qualiquanti_color,
                 size = 3) +
      geom_vline(xintercept = 0,
                 linetype = "solid",
                 colour = "black") +
      geom_hline(yintercept = 0,
                 linetype = "solid",
                 colour = "black") +
      add_geom_text(DF,
                    active_entity,
                    colors = c(active_color, qualiquanti_color)) +
      scale_colour_gradient(
        low = rgb(355 / 355, 355 / 355, 355 / 355, 1),
        high = rgb(0 / 355, 0 / 355, 0 / 355, 1),
        limits = c(0,1)
      ) +
      add_qualiquanti_color(DF_qualiquanti,
                            qualiquanti_color) +
      labs(title = active_entity,
           x = active_dims[1],
           y = active_dims[2])

    base +
      remove_panel_grid() +
      remove_panel_background() +
      justify_plot_title("center") +
      margin_y_right(7) +
      margin_x_top(7)
  }


#' Add Layer for Geometric Objects
#'
#' This utility adds a text geometric object, conditional to which entity is
#'  currently active (variables vs individuals).
#' We only want labels when plotting variables.
#'
#' @param data_ Data frame
#' @param active_entity String. Currently active entity (variables vs
#'  individuals)
#' @param color Hexadecimal. The color of the text
#'
#'
add_geom_text <- function(data_, active_entity, colors) {
  text_color <- ifelse(data_$qualiquanti == 1, colors[2], colors[1])
  if (active_entity == "Variables") {
    ggrepel::geom_text_repel(data = data_,
                             aes(dim_x,
                                 dim_y,
                                 label = index),
                             color = text_color)

  }
}

#' Create Ggplot Object for Eigenvalues
#'
#'
#' @param eigenvalues Eigenvalues of current MCA results. Eigenvalues can be
#'   expressed either as absolute values, or as cumulative percentage of
#'   explained variance
#'

create_eigen_ggplot <- function(eigenvalues) {
  len <- min(9, length(names(eigenvalues)))
  data_ <- list(dims = names(eigenvalues)[1:len],
                eigens = unname(eigenvalues)[1:len])

  base <- ggplot(data.frame(data_)) +
    geom_col(aes(dims, eigens), fill = "#000000")

  base +
    remove_panel_background() +
    remove_panel_grid() +
    margin_y_right(7) +
    margin_x_top(7) +
    set_plot_title("Explained variance") +
    justify_plot_title("center")
}

add_qualiquanti_color <- function(data_, colour) {
  geom_point(data = data_, aes(dim_x, dim_y), color = colour)
}



# Validation  -------------------------------------------------------------

#' Get Validated Data Frame
#'
#' Take a data frame, and remove all columns belonging to a predefined set of
#'  data types.
#'
#' @param DF A data frame
#'
#' @return A data frame

get_validated_DF <- function(DF) {
  to_keep_idx <- DF %>%
    map_lgl(is_to_keep)
  to_keep <- names(DF)[to_keep_idx]
  DF <- select(DF, all_of(to_keep))
  return(DF)
}


is_to_keep <- function(array_) {
  type <- typeof(array_)
  types_to_remove <- c("numeric", "complex", "double", "raw", "list", "closure",
                       "special", "builtin", "environment", "S4")
  return(ifelse(type %in% types_to_remove, FALSE, TRUE))
}

