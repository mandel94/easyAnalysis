# Define styles
# container_fluid_style <- "
#                          .container-fluid {
#                             padding-left: 5px;
#                             padding-rigth: 5px;
#                            }
#                          "

busy_message_style <-  "
                       #loadmessage {
                         top: 0px;
                         height: 20px;
                         width: 100%;
                         margin:0;
                         padding: 0px 0px 0px 0px;
                         text-align: center;
                         font-weight: bold;
                         font-size: 100%;
                         color: #000000;
                         z-index: 105;
                        }
                       "

selectize_style <- "
                   .selectize-input {
                     height: 100px;
                    }
                   "

#
# well_title_style <- "
#                     .well-title {
#                     font-size: 20px;
#                     font-style: bold;
#                     }
#                     "



#' Apply CSS Styles
#'
#' This function applies custom defined css style.
#'
#'
#' @return Tag List, each one containing a style tag applied to the head of the
#'  html

apply_custom_css <- function() {
  styles <- c(selectize_style,
              busy_message_style)


  styles_list <- vector("list", length(styles))
  i <- 1
  for (style in styles) {
    styles_list[[i]] <- tags$head(tags$style(type = "text/css",
                                             style))
    i <- i + 1
  }

  # Return tagList
  tagList(styles_list)
}
