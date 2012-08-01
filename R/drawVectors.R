#' plot the bearings for a set of data
#'
#' @param f a formula (with no LHS) specifying the bearing and groupings
#' @param data a data frame or spatial data frame with the coordinates, bearings, and groupings
#' @param subset logical vector to subset the data
#' @param length length of line segments to draw
#' @param ... extra parameters passed to the plot method
#'
#' @export
drawVectors <- function(f,data,subset=TRUE,length,...){

  xybg=.getxybg(f,data,subset)
  theta = theta(xybg$bearing)

  group = interaction(xybg[,-(1:3)],drop=FALSE,sep="|")
  
  x=xybg$x
  y=xybg$y
  if(missing(length)){
    p = par()$usr
    diag = sqrt((p[2]-p[1])^2+(p[3]-p[4])^2)
    length=diag
  }
  dx = x+length*cos(theta)
  dy = y+length*sin(theta)
  segments(x,y,dx,dy,col=as.numeric(group))

}
