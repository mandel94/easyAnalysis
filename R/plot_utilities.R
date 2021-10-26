#' Return a thema function for customizing font
#'
#'
#' @param font_family Font-family to apply
#'
font_family <- function(font_family, ...) {
  theme(text = element_text(family = font_family), ...)
}

# Set right margin of y axis title
margin_y_right <- function(mg) {
  theme(axis.title.y = element_text(margin = margin(r = mg)))
}

margin_x_top <- function(mg) {
  theme(axis.title.x = element_text(margin = margin(t = mg)))
}


remove_panel_background <- function() {
  theme(panel.background = element_blank())
}


remove_panel_grid <- function() {
  theme(panel.grid = element_blank())
}


set_plot_title <- function(title) {
  labs(title = title)
}

#' Justify plot title
#'
#' @param position Admitted values are:
#' \itemize {
#'  \item "left"
#'  \item "center"
#'  \item "right}

justify_plot_title <- function(position) {
  if (position == "right")
    h <- 1
  else if (position == "left")
    h <- 0
  else if (position == "center")
    h <- 0.5

  theme(plot.title = element_text(hjust = h))
}

#' Align plot title
#'
#' @param position Admitted values are:
#' \itemize {
#'  \item "left"
#'  \item "center"
#'  \item "right}

aling_plot_title <- function(position) {
  if (position == "right")
    v <- 1
  else if (position == "left")
    v <- 0
  else if (position == "center")
    v <- 0.5

  theme(plot.title = element_text(vjust = v))
}


