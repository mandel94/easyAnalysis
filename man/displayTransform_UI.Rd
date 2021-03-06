% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/displayTransformModule.R
\name{displayTransform_UI}
\alias{displayTransform_UI}
\title{Module for Displaying Crafted Data Frame (on Transform Navbar Page)}
\usage{
displayTransform_UI(id)
}
\arguments{
\item{id}{Module's ID for namespacing}
}
\value{
\code{\link[shiny]{tagList}} that can be passed to a shinyUI function
}
\description{
This is module will be used to display the data frame crafted by the user.
In this circumstance, crafting is the result of two phases:
\describe{
\item{select}{the selection of variables from the list resulting from imported data}
\item{transform}{the transformations applied by the user to the original data}
}
This documentation refers to the UI-side function of the module. For the documentation
about the server-side function, got to \code{\link{displayData_server}}
}
