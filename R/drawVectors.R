drawVectors <- function(x,y,theta,length,...){
  if(missing(length)){
    p = par()$usr
    diag = sqrt((p[2]-p[1])^2+(p[3]-p[4])^2)
    length=diag
  }
  dx = x+length*cos(theta)
  dy = y+length*sin(theta)
  segments(x,y,dx,dy,...)

}
