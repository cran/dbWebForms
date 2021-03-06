% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbWebForms.R
\name{namesToLabels}
\alias{namesToLabels}
\title{Convert strings to title case, splitting strings into separate words based on a separator.}
\usage{
namesToLabels(x, split = "_")
}
\arguments{
\item{x}{A string.}

\item{split}{A string, used to split x into constituent words to be converted to title case.}
}
\value{
A string, converted to title case with split words separated with a space character.
}
\description{
Convert strings to title case, splitting strings into separate words based on a separator.
}
\examples{
namesToLabels("date_of_birth", split = "_")
}
