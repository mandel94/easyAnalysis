# DF <- data.frame(read_inst("inst/test-plot_DF_var"))
# DF$qualiquanti <- c(rep(1, 10), rep(0, nrow(DF)-10))
# DF$text_col <- c(rep("red", 10),
#                  rep(
#                    "green", nrow(DF) - 10
#                  ))
#
# ggplot(DF) +
#   geom_point(aes(dim_x, dim_y, color = cos2_x)) +
#   geom_text(aes(dim_x, dim_y, label = index), color = DF$text_col)
