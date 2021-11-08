#' @import dplyr

## code to prepare `DATASET` dataset goes here


usethis::use_data(mental_health_tech, overwrite = TRUE)

DF <- data.table::fread("data-raw/survey.csv")
perc_NA <- 0.1

to_keep_idx <- DF %>%
  purrr::map_dbl(function(.)
    sum(is.na(.)) / length(.)) %>%
  purrr::map_lgl(function(.)
    . <= perc_NA)

names_to_keep <- names(DF)[to_keep_idx]

mental_health_tech <- DF[, ..names_to_keep]
write.csv(mental_health_tech, "data/mental_health_tech.csv")
