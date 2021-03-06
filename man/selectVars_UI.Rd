% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectVarsModule.R
\name{selectVars_UI}
\alias{selectVars_UI}
\title{Select Variables to display.}
\usage{
selectVars_UI(id)
}
\arguments{
\item{id}{Modules'ID}
}
\value{
\code{\link[shiny]{tagList}} that can be passed to a shinyUI function
}
\description{
This module allows to select which variables from imported data will be
included in the crafted data frame.
This documentation refers to the UI-side function of the module. For the documentation
about the server-side function, got to \code{\link{selectVars_server}}
}
