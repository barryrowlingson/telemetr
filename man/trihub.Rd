\name{trihub}
\alias{trihub}
\title{triangulate by Huber method}
\usage{
  trihub(xytower, bearing, ijob = 1, kappa = 1, sd = 2.5)
}
\arguments{
  \item{xytower}{2 column matrix of xy coords}

  \item{bearing}{azimuth angles in degrees}

  \item{ijob}{whether to fit kappa}

  \item{kappa}{value of kappa}

  \item{sd}{value of sd

  Uses the Huber method.}
}
\value{
  A named vector with x and y values, variance matrix and
  error indicator
}
\description{
  triangulate by Huber method
}

