#' make some fake data
#'
#' @param ntowers number of towers
#' @param xlim x range
#' @param ylim y range
#' @param noise angular noise in degrees
#'
#' Place a critter at a random uniform point in the xlim,ylim box. Place ntowers
#' randomly uniform in the box. Compute angles from the towers to the critter. Add
#' some random normal angular noise with mean 0 and se=noise to the angles to
#' get the simulated bearings.
#'
#' @return a data frame with x, y, thetaTrue, bearing, and theta values. It
#' also has a 'critter' attribute which is the true location of the critter.
#' Note that thetaTrue and theta are conventional angles from the x=0 line, and
#' bearing is a reading in degrees from North.
#'
#' @export
makeTriData <- function(ntowers, xlim=c(0,1),ylim=c(0,1),noise=5){
  ranpt = function(n,lim){runif(n,lim[1],lim[2])}
  xpt = function(n){ranpt(n,xlim)}
  ypt = function(n){ranpt(n,ylim)}
  
  xy = data.frame(
    x=xpt(ntowers),
    y=ypt(ntowers)
    )
  critter = c(xpt(1),ypt(1))

  xy$thetaTrue = atan2(critter[2]-xy$y,critter[1]-xy$x)
  noiseR = noise*pi/180
  xy$bearing = bearing(xy$theta+rnorm(ntowers,0,noiseR))
  xy$theta = theta(xy$bearing)
  attr(xy,"critter")=critter
  xy
                  
    
}

#' make some fake data for a number of groups
#'
#' @param ntowers number of towers giving readings in each group
#' @param animals a group variable, typically animal id
#' @param dates a date
#'
#' for each unique combination of animal and date, we call \code{\link{makeTriData}}
#' and return a spatial points data frame with the animal and date variables as well
#' as the true and noisy directions. The coordinates of the critter are not preserved
#' (but can be got pretty accurately from the thetaTrue values and at least two readings)
#'
#' @return A Spatial Points Data Frame of simulated data.
#' @export
makeMoreTriData <- function(ntowers=3,animals=1:4,dates=as.Date("2001/12/1")+0:1){
  force(ntowers)
  makeThem = function(...){
    makeTriData(ntowers)
  }
  d = adply(expand.grid(animal=animals,date=dates),1,makeThem)
  coordinates(d)=~x+y
  d
}
