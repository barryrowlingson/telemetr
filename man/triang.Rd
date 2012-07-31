\name{triang}
\alias{triang}
\title{triangulate positions from locations and bearings}
\usage{
  triang(f, data, method = "mle", subset = TRUE, ...)
}
\arguments{
  \item{f}{a formula (with no LHS) specifying the bearing
  and groupings}

  \item{data}{a data frame or spatial data frame with the
  coordinates, bearings, and groupings}

  \item{method}{name of the method to use}

  \item{subset}{logical vector to subset the data}

  \item{...}{extra parameters passed to the individual
  methods

  The formula is minimally of the form ~bearing if every
  row of the data frame is to be used to compute one
  location, or of the form ~bearing|f1+f2+f3 if the
  specified variables after the | are used to construct
  groups within which the rows consist of one set of
  observations.

  If the data frame is not a Spatial Points Data Frame,
  then the coordinates will be taken from columns named 'x'
  and 'y'.

  For specific information about parameters for the
  different methods, consult the individual functions:
  \code{\link{trimle}}, \code{\link{trihub}},
  \code{\link{triand}}, \code{\link{trirmr}}. These
  functions only work on a single set of observations.}
}
\value{
  A spatial data frame of each position estimate for data
  within each group.
}
\description{
  triangulate positions from locations and bearings
}

