
### a bearing is: North = 0, East = 90, South = 180, West = 270
### a theta is East = 0, South = -pi/2, West = +/-pi, North = +pi/2

bearing <- function(theta){
  return ( (90-theta*180/pi ) %% 360)
}

theta <- function(bearing){
  return( ((90.-abs(bearing))*(pi/180.)) %% (2*pi))
}
