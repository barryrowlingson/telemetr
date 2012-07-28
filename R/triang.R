triang <- function(f,data,method="mle",subset=TRUE){

  ## get x,y,bearing,group data frame
  df = .getxybg(f,data,subset=subset)

  ## group on all but the first three columns (x,y,bearing)
  ddply(df,names(df)[-(1:3)],testf)

  
}

testf=function(df){
  data.frame(x=mean(df$x),y=mean(df$y))
}

.getxybg <- function(f,data,subset=TRUE){
  parsed = latticeParseFormula(f,data=data,subset=subset)
  if(!is.null(parsed$left)){
    stop("LHS of formula must be empty")
  }
  bearing = parsed$right
  if(is.null(parsed$condition)){
    ## its all one group
    parsed$condition = data.frame(ID=rep(1,length(parsed$right)))
  }
  coords = .getCoords(data[subset,,drop=FALSE])
  df = data.frame(x=coords[,1],y=coords[,2],
    bearing=bearing,
    parsed$condition
    )
  df
}

.getCoords <- function(data){
  coords = NULL
  if(inherits(data,"Spatial")){
    coords = coordinates(data)
  }else{
    if("x" %in% names(data)){
      if("y" %in% names(data)){
        coords = data.frame(x=data$x,y=data$y)
        warning("coordinates got from x and y columns")
      }
    }
  }
  if(is.null(coords)){
    stop("Can't get coords from data frame")
  }
  return(coords)
}
