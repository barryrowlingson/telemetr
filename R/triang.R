triang <- function(f,data,method="mle",subset=TRUE,...){

  ## get x,y,bearing,group data frame
  df = .getxybg(f,data,subset=subset)

  methodF = switch(method,
    mle = .domle,
    and = .doand,
    hub = .dohub,
    rmr = .dormr,
    stop("invalid method ",method," given")
    )
  
  ## group on all but the first three columns (x,y,bearing)
  triangs = ddply(df,names(df)[-(1:3)],methodF,...)
  coordinates(triangs)=~x+y
  triangs
  
}

.getxybg <- function(f,data,subset=TRUE){
  coords = .getCoords(data[subset,,drop=FALSE])
  if(inherits(data,"Spatial")){
    data=data@data
  }
  parsed = latticeParseFormula(f,data=data,subset=subset)
  if(!is.null(parsed$left)){
    stop("LHS of formula must be empty")
  }
  bearing = parsed$right
  if(is.null(parsed$condition)){
    ## its all one group
    parsed$condition = data.frame(ID=rep(1,length(parsed$right)))
  }

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
