

.dormr=function(df,...){
  fit = trirmr(cbind(df$x,df$y),df$bearing)
  fit["npts"]=nrow(df)
  fit
}


#' triangulate by repeated median method
#'
#' @param xytower 2 column matrix of xy coords
#' @param bearing azimuth angles in degrees
#'
#' for each bearing compute the intersection with all the other bearings,
#' and take the median x and median y of those intersections. This gives one
#' x and y for each bearing. Then take the median of those x and y coordinates
#' to get a single value. Compute the standard error by a jack-knife procedure.
#'
#' @return A named vector of x and y coords
#' @export
trirmr <- function(xytower,bearing){

  ntower = nrow(xytower)
  theta = theta(bearing)

  ri = rayIntersections(xytower,bearing)
  jack = .jacknife(ri)
  return(c(.xymedianmedian(ri),jack))
}

.xymedianmedian <- function(ri){
  m1 = ddply(ri,~i,function(z){c(x=median(z$x),y=median(z$y))})
  return(c(x=median(m1$x),y=median(m1$y)))
}

.jacknife <- function(ri){
  xy = data.frame(x=NULL,y=NULL)
  for(i in unique(ri$i)){
    jack = ri$i == i | ri$j == i
    riX = ri[!jack,,drop=FALSE]
    mm = .xymedianmedian(riX)
    xy=rbind(xy,data.frame(x=mm["x"],y=mm["y"]))
  }
  xy = subset(xy,!is.na(xy[,1]))
  n = length(unique(ri$i))
  n = nrow(xy)
  jack.se.x = sqrt(((n-1)/n) * sum((xy[,1]-mean(xy[,1]))^2))
  jack.se.y = sqrt(((n-1)/n) * sum((xy[,2]-mean(xy[,2]))^2))

  c(se.x=jack.se.x,se.y=jack.se.y,cor=cor(xy[,1],xy[,2]))
}

#' compute ray intersections
#'
#' @param xy 2-column matrix of coordinates
#' @param bearing vector of azimuth measurements in degrees from north
#'
#' @return a four column data frame with x and y coordinates of all intersections,
#'         and i and j integer indices of the corresponding inputs. 
#'
#' @export
rayIntersections <- function(xy,bearing){
  theta = theta(bearing)
  xy = cbind(xy,xy[,1]+cos(theta),xy[,2]+sin(theta))
  crosses = data.frame(x=NULL,y=NULL,i=NULL)
  for(i in 1:nrow(xy)){
    x3 = xy[,1]; x4 = xy[,3]
    y3 = xy[,2]; y4 = xy[,4]
    x1 = x3[i]; x2=x4[i]
    y1 = y3[i]; y2=y4[i]
    j = (1:nrow(xy))[-i]
    x3=x3[-i];x4=x4[-i]
    y3=y3[-i];y4=y4[-i]

    na = (x4-x3)*(y1-y3)-(y4-y3)*(x1-x3)
    da = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)

    nb = (x2-x1)*(y1-y3)-(y2-y1)*(x1-x3)
    db = (y4-y3)*(x2-x1)-(x4-x3)*(y2-y1)

    x = x1 + (na/da)*(x2-x1)
    y = y1 + (na/da)*(y2-y1)
    ok = sign(na/da)==1 & sign(nb/db)==1
    crosses=rbind(crosses,data.frame(x=x[ok],y=y[ok],i=rep(i,sum(ok)),j=j[ok]))
  }

  crosses

}
