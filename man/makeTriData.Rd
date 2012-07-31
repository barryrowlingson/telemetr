\name{makeTriData}
\alias{makeTriData}
\title{make some fake data}
\usage{
  makeTriData(ntowers, xlim = c(0, 1), ylim = c(0, 1),
    noise = 5)
}
\arguments{
  \item{ntowers}{number of towers}

  \item{xlim}{x range}

  \item{ylim}{y range}

  \item{noise}{angular noise in degrees

  Place a critter at a random uniform point in the
  xlim,ylim box. Place ntowers randomly uniform in the box.
  Compute angles from the towers to the critter. Add some
  random normal angular noise with mean 0 and se=noise to
  the angles to get the simulated bearings.}
}
\value{
  a data frame with x, y, thetaTrue, bearing, and theta
  values. It also has a 'critter' attribute which is the
  true location of the critter. Note that thetaTrue and
  theta are conventional angles from the x=0 line, and
  bearing is a reading in degrees from North.
}
\description{
  make some fake data
}

