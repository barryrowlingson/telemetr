

.doand=function(df,...){
  fit = triand(cbind(df$x,df$y),df$bearing)
  fit["npts"]=nrow(df)

#  data.frame(x=fit$coor[1],y=fit$coor[2])
  fit
}


#' triangulate by Andrew's method
#'
#' @param xytower 2 column matrix of xy coords
#' @param bearing azimuth angles in degrees
#' @param ijob whether to fit kappa
#' @param kappa value of kappa
#' @param sd value of sd
#' @return A named vector with x and y values, variance matrix and error indicator
#' @export
triand <- function(xytower,bearing,ijob=1,kappa=1,sd=2.5){

  ntower = nrow(xytower)
  itower = 1:ntower
  xytower=t(as.matrix(xytower))
  # stopifnot(ntower==3)
  coor = c(99,99)
  vc = matrix(-1,2,2)
  az = (90.-abs(bearing))*(pi/180.)

  res = .Fortran("triand",
    tutm = as.double(xytower),
    az = as.double(az),
    itower = as.integer(itower),
    ntower = as.integer(ntower),
    coor = as.double(coor),
    sd = as.double(sd),
    kappa = as.double(kappa),
    vc = as.double(vc),
    ijob = as.integer(ijob),
    ierr = as.integer(0)
    )

  covmat = matrix(c(res$vc[1],res$vc[2],res$vc[3],res$vc[4]),ncol=2)
  cor = cov2cor(covmat)[1,2]
  se.xy = sqrt(diag(covmat))

  d = unlist(list(x=res$coor[1],y=res$coor[2],
    sd=res$sd,kappa=res$kappa,
    cor = cor,
    se.x=se.xy[1],
    se.y=se.xy[2],
    ijob=res$ijob,
    err=res$ierr))
  return(d)
}
