makeData <- function(ntowers, xlim=c(0,1),ylim=c(0,1),noise=5){
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
