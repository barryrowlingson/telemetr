\name{makeMoreTriData}
\alias{makeMoreTriData}
\title{make some fake data for a number of groups}
\usage{
  makeMoreTriData(ntowers = 3, animals = 1:4,
    dates = as.Date("2001/12/1") + 0:1)
}
\arguments{
  \item{ntowers}{number of towers giving readings in each
  group}

  \item{animals}{a group variable, typically animal id}

  \item{dates}{a date

  for each unique combination of animal and date, we call
  \code{\link{makeTriData}} and return a spatial points
  data frame with the animal and date variables as well as
  the true and noisy directions. The coordinates of the
  critter are not preserved (but can be got pretty
  accurately from the thetaTrue values and at least two
  readings)}
}
\value{
  A Spatial Points Data Frame of simulated data.
}
\description{
  make some fake data for a number of groups
}

