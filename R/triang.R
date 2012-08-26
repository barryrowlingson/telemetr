#' triangulate positions from locations and bearings
#'
#'
#' @param f a formula (with no LHS) specifying the bearing and groupings
#' @param data a data frame or spatial data frame with the coordinates, bearings, and groupings
#' @param method name of the method to use
#' @param subset logical vector to subset the data
#' @param ... extra parameters passed to the individual methods
#'
#' The formula is minimally of the form ~bearing if every row of the data frame
#' is to be used to compute one location, or of the form ~bearing|f1+f2+f3 if
#' the specified variables after the | are used to construct groups within which
#' the rows consist of one set of observations.
#'
#' If the data frame is not a Spatial Points Data Frame, then the coordinates will
#' be taken from columns named 'x' and 'y'.
#'
#' For specific information about parameters for the different methods, consult the
#' individual functions: \code{\link{trimle}}, \code{\link{trihub}}, \code{\link{triand}},
#' \code{\link{trirmr}}. These functions only work on a single set of observations.
#'
#' @return A spatial data frame of each position estimate for data within each group.
#' @useDynLib telemetr
#' @export
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
  proj4string(triangs) = proj4string(data)
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
