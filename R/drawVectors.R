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
