\name{rayIntersections}
\alias{rayIntersections}
\title{compute ray intersections}
\usage{
  rayIntersections(xy, bearing)
}
\arguments{
  \item{xy}{2-column matrix of coordinates}

  \item{bearing}{vector of azimuth measurements in degrees
  from north}
}
\value{
  a four column data frame with x and y coordinates of all
  intersections, and i and j integer indices of the
  corresponding inputs.
}
\description{
  compute ray intersections
}

