#' Module for Adding 'shiny-busy' Messages
#'
#' This module improves User Experience by adding "Loading..." messages when
#'  shiny gets busy making computations.
#'
#' @param id Modules'ID

addBusyMessage <- function(ns) {

  tagList(
    # tags$head(tags$style(type = "text/css", "
    #                      #loadmessage {
    #                        position: ;
    #                        top: 0px;
    #                        left: 0px;
    #                        width: 100%;
    #                        padding: 5px 0px 5px 0px;
    #                        text-align: center;
    #                        font-weight: bold;
    #                        font-size: 100%;
    #                        color: #000000;
    #                        background-color: #CCFF66;
    #                        z-index: 105;
    #                        }
    #                      ")),
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     tags$div('Loading...',
                              id = 'loadmessage',
                              style = 'background-color: #CCFF66;')),
    conditionalPanel(condition = "!($('html').hasClass('shiny-busy'))",
                     tags$div('',
                              id = 'loadmessage',
                              style = 'background-color: #FFFFFF;'))
  )
}



