\name{trirmr}
\alias{trirmr}
\title{triangulate by repeated median method}
\usage{
  trirmr(xytower, bearing)
}
\arguments{
  \item{xytower}{2 column matrix of xy coords}

  \item{bearing}{azimuth angles in degrees

  for each bearing compute the intersection with all the
  other bearings, and take the median x and median y of
  those intersections. This gives one x and y for each
  bearing. Then take the median of those x and y
  coordinates to get a single value. Compute the standard
  error by a jack-knife procedure.}
}
\value{
  A named vector of x and y coords
}
\description{
  triangulate by repeated median method
}

