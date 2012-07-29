

.dormr=function(df,...){
  fit = trirmr(cbind(df$x,df$y),df$bearing)
  fit["npts"]=nrow(df)
  fit
}


#' triangulate by repeated median method
#'
#' @param xytower 2 column matrix of xy coords
#' @param az azimuth angles in degrees
#' @export
trirmr <- function(xytower,bearing){

  ntower = nrow(xytower)
  theta = theta(bearing)

  ri = rayIntersections(xytower,bearing)
  m1 = ddply(ri,~pt,function(z){c(x=median(z$x),y=median(z$y))})

  d = c(x=median(m1$x),y=median(m1$y))
  return(d)
}

#' compute ray intersections
#'
#' @export
rayIntersections <- function(xy,bearing){
  theta = theta(bearing)
  xy = cbind(xy,xy[,1]+cos(theta),xy[,2]+sin(theta))
  crosses = data.frame(x=NULL,y=NULL,pt=NULL)
  for(i in 1:nrow(xy)){
    x3 = xy[,1]; x4 = xy[,3]
    y3 = xy[,2]; y4 = xy[,4]
    x1 = x3[i]; x2=x4[i]
    y1 = y3[i]; y2=y4[i]
    x3=x3[-i];x4=x4[-i]
    y3=y3[-i];y4=y4[-i]

    na = (x4-x3)*(y1-y3)-(y4-y3)*(x1-x3)
    da = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)

    nb = (x2-x1)*(y1-y3)-(y2-y1)*(x1-x3)
    db = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)

    x = x1 + (na/da)*(x2-x1)
    y = y1 + (na/da)*(y2-y1)
    ok = sign(na/da)==1 & sign(nb/db)==1
    crosses=rbind(crosses,data.frame(x=x[ok],y=y[ok],pt=rep(i,sum(ok))))
  }

  crosses

}
