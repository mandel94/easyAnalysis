% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{add_labeled_values}
\alias{add_labeled_values}
\title{Add Labeled Rownames}
\usage{
add_labeled_values(DF, labeled_values)
}
\arguments{
\item{DF}{A data frame}

\item{labeled_values}{Array, with values labeled by category}
}
\value{
A data frame, the same as the input one, but with rownames modified
with the labeled version of variable values.
}
\description{
When applying MCA, original category names are pre-appended to the
answer only for binary variables. This function is used for changing the row
names of the input DF, pre-appending the category names for all types of
variable.
}
